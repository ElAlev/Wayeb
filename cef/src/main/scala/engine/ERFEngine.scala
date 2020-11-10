package engine

import profiler.WtProfiler
import stream.GenericEvent
import com.typesafe.scalalogging.LazyLogging
import db.DBConnector
import fsm.FSMInterface
import fsm.runtime.{MatchDump, Run, RunPool}
import model.forecaster.runtime.{ForecasterRun, ForecasterRunFactory}
import stream.array.EventStream
import stream.source.{EndOfStreamEvent, StreamListener}
import ui.ConfigUtils
import utils.Progressor
import workflow.provider.{FSMProvider, ForecasterProvider}

object ERFEngine {
  /**
    * Create an engine with some default parameter values (retrieved from application.conf).
    * Only FSM provider and Predictor provider given.
    *
    * @param fsmp The provider for the FSM(s) that run recognition.
    * @param predProvider The provider for the predictor(s) that emit forecasts.
    * @return The engine with the default parameter values.
    */
  def apply(
             fsmp: FSMProvider,
             predProvider: ForecasterProvider
           ): ERFEngine = {
    apply(fsmp             = fsmp,
          predProvider     = predProvider,
          predictorEnabled = ConfigUtils.defaultPredictorEnabled,
          expirationDeadline   = ConfigUtils.defaultExpiration,
          collectStats     = ConfigUtils.defaultCollectStats,
          finalsEnabled    = ConfigUtils.defaultFinalsEnabled,
          distance         = ConfigUtils.defaultDistance,
          show = ConfigUtils.defaultShowMatchesForecasts
    )
  }

  /**
    * Constructor for engine.
    *
    * @param fsmp The provider for the FSM(s) that run recognition.
    * @param predProvider The provider for the predictor(s) that emit forecasts.
    * @param predictorEnabled Determine whether the predictor will be enabled or not.
    * @param expirationDeadline Determines when the collection of the runs is triggered.
    * @param collectStats Boolean parameter that determines whether statistics should be collected and estimated
    *                     in runtime.
    * @param finalsEnabled Boolean parameter that determines whether the final states of the FSMs should emit forecasts
    *                      as well.
    * @param distance A parameter (Tuple2, (minDistance,maxDistance)) that determines when forecasts should be emitted.
    * @param show Determines whether complex event matches and forecasts are to be displayed or not.

    * @return An ERF engine.
    */
  def apply(
             fsmp: FSMProvider,
             predProvider: ForecasterProvider,
             predictorEnabled: Boolean,
             expirationDeadline: Long,
             collectStats: Boolean,
             finalsEnabled: Boolean,
             distance: (Double, Double),
             show: Boolean
           ): ERFEngine = {
    new ERFEngine(fsmp,
                  predProvider,
                  predictorEnabled,
                  expirationDeadline,
                  collectStats,
                  finalsEnabled,
                  distance,
                  show
    )
  }

}

/**
  * This is the main engine that runs recognition and forecasting on a stream.
  * The engine is a stream listener (stream.source.StreamListener) that receives events from
  * a stream source (stream.source.StreamSource).
  *
  * @param fsmProvider The provider for the FSM(s) that run recognition.
  * @param predProvider The provider for the predictor(s) that emit forecasts.
  * @param predictorEnabled Determine whether the predictor will be enabled or not.
  *                         If false, no forecasts will be emitted (only recognition).
  * @param expirationDeadline Determines when the collection of the runs is triggered.
  *                       Collection takes place every expirationTime time units.
  *                       Collections reclaim from memory stale runs that have not been been updated since
  *                       currentTimestamp - expirationTime.
  *                       If -1, no collections take place, i.e., all runs remain in memory "for ever".
  *                       CAUTION: enabling collection of runs may make recognition incomplete, i.e.,
  *                       not all matches will be detected, essentially restricting matches to occur within
  *                       a window of expirationTime time units.
  * @param collectStats Boolean parameter that determines whether statistics should be collected and estimated
  *                     in runtime. Used mostly for testing and evaluation. In real environments, collecting
  *                     statistics may decrease throughput.
  * @param finalsEnabled Boolean parameter that determines whether the final states of the FSMs should emit forecasts
  *                      as well. If false, only the non-final states emit forecasts.
  * @param distance A parameter (Tuple2, (minDistance,maxDistance)) that determines when forecasts should be emitted.
  *
  *                 If minDistance==maxDistance==-1, forecasts are emitted eagerly, whenever possible (i.e., whenever
  *                 the current state of a FSM can provide a valid forecast, forecast interval satisfying the confidence
  *                 threshold and max spread constraints).
  *
  *                 If minDistance!=-1 and minDistance < 1.0 (same for maxDistance), then forecasts are emitted only
  *                 from states whose remaining percentage is between minDistance and maxDistance.
  *                 The remaining percentage of a state is the ratio of its shortest path length to a final divided by
  *                 the maximum shortest path from all states. It gives an estimate of how close a state is to
  *                 completion. A remaining percentage of 0.0 means that the automaton is in a final state. A
  *                 remaining percentage of 1.0 means that it is in one of its start states.
  *
  *                 If minDistance!=-1 and minDistance >= 1.0 (same for maxDistance), forecasts are emitted only when
  *                 the current event's timestamp has a temporal distance from the next CE in the stream that is equal
  *                 to the value of the parameter.
  *                 CAUTION: This setting (with distances>=1.0) should be used only when one pattern is provided via
  *                 fsmProvider. Does not work for multiple patterns. Moreover, the input stream must have been
  *                 pre-processed so that each input event has an extra attribute, called "nextCETimestamp", providing
  *                 the timestamp of the next CE. See fsm.runtime.Run#isEmitting(stream.GenericEvent).
  * @param show Determines whether complex event matches and forecasts are to be displayed or not.
  */

class ERFEngine private (
                          fsmProvider: FSMProvider,
                          predProvider: ForecasterProvider,
                          predictorEnabled: Boolean,
                          expirationDeadline: Long,
                          collectStats: Boolean,
                          finalsEnabled: Boolean,
                          distance: (Double, Double),
                          show: Boolean
                        ) extends StreamListener with LazyLogging {

  logger.info("Initializing engine...")

  private val singlePartitionVal = ConfigUtils.singlePartitionVal
  private val predList = predProvider.provide()
  private val fsmList = fsmProvider.provide().zip(predList).map(x => {
    if (distance._1 != -1.0 & distance._1 < 1.0) {
      if (x._1.remainingPercentage.isEmpty) x._1.estimateRemainingPercentage
      x._1
    } else x._1
  })
  private val runPool = RunPool(fsmList, expirationDeadline, distance, show)
  private val md = new MatchDump()
  private val predFactory = ForecasterRunFactory(predList, collectStats, finalsEnabled)
  private var predictorRuns: Map[Int, List[ForecasterRun]] = fsmList.map(fsm => fsm.getId).
    map(id => id -> List.empty[ForecasterRun]).toMap

  private var matchesNo = 0
  private var execTime: Long = 0
  private var lastCollectTime: Long = 0
  private var streamSize: Int = 0
  private var profiler = WtProfiler()

  logger.info("Initialization complete.")

  /**
    * Processes a single event from the input stream.
    *
    * @param event The current event (last event emitted from the stream source).
    */
  override def newEventEmitted(event: GenericEvent): Unit = {
    event match {
      /**
        * Every stream must contain an EndOfStreamEvent as its last event.
        * Upon the arrival of such an event, statistics are estimated and provided through a profiler.
        */
      case _: EndOfStreamEvent => {
        profiler = shutdown()
        logger.info("Use getProfiler to retrieve stats.")
      }
      case _ => {
        streamSize += 1
        processEvent(event)
      }
    }
  }

  /**
    * Processing of a single event.
    *
    * @param event The event to process.
    */
  private def processEvent(event: GenericEvent): Unit = {
    val t1 = System.nanoTime()
    val currentTimestamp = event.timestamp
    // All FSMs process the event.
    val det = fsmList.map(f => processEvent(event, f))
    matchesNo += det.count(d => d._1)
    // If it's time to collect stale runs, do so.
    if (shouldCollect(currentTimestamp, lastCollectTime)) {
      runPool.runsCollect(currentTimestamp)
      lastCollectTime = currentTimestamp
    }
    //execTime += det.map(d => d._2).sum
    val t2 = System.nanoTime()
    execTime += (t2 - t1)
  }

  /**
    * Method to process the whole stream, if it is provided as an ArrayBuffer.
    * Deprecated method, retained for testing. Useful for showing the progress of processing!
    *
    * @param eventStream The event stream provided as an ArrayBuffer (see stream.array.EventStream).
    * @return A profiler holding statistics.
    */
  @deprecated
  def processStream(eventStream: EventStream): WtProfiler = {
    streamSize = eventStream.getSize
    logger.info("Running Wayeb for " + streamSize + " events")
    val progressor = Progressor("ERFEngine", streamSize, 5)
    for (i <- 0 until streamSize) {
      val e = eventStream.getEvent(i)
      processEvent(e)
      progressor.tick
    }
    shutdown()
  }

  /**
    * Processing of a single event by a single FSM.
    *
    * @param event The event to be processed.
    * @param thisFsm The FSM to process the event.
    * @return True if the event led to a CE detection. Also process time for run in nanoseconds.
    */
  private def processEvent(
                            event: GenericEvent,
                            thisFsm: FSMInterface
                          ): (Boolean, Long) = {
    var detected = false
    /**
      * First find the appropriate run.
      * Exactly one run per FSM is possible (and all FSMs attempt to process the event).
      */
    val r = findRun(event, thisFsm)
    val pt = r.processEvent(event)
    detected = r.ceDetected
    //RunPool.runsCollect(e.getTimestamp)
    (detected, pt)
  }

  /**
    * Method that finds the appropriate run of an FSM that should do the actual processing of the event.
    *
    * @param event The event to be processed.
    * @param thisFsm The FSM for which we are trying to find an appropriate run.
    * @return The appropriate run.
    */
  private def findRun(
                       event: GenericEvent,
                       thisFsm: FSMInterface
                     ): Run = {
    val id = thisFsm.getId
    val partitionAttribute = thisFsm.getPartitionAttribute
    // First retrieve the value of the  partition attribute,
    // i.e., the attribute by which the input stream is to be partitioned.
    val av = if (partitionAttribute.equalsIgnoreCase(singlePartitionVal)) singlePartitionVal
    else event.getValueOf(partitionAttribute).toString
    // If the pool of runs already has a run with this partition value (for the ID of the FSM we
    // are examining), then get this run.
    if (runPool.existsRunWithAttributeVal(id, av)) {
      runPool.getRunByAttribute(id, av)
    } // Otherwise, create a new run with this partition value.
    else {
      val r1 = runPool.checkOut(id, av, event.timestamp)
      r1.register(md) // Register the MatchDump with the new run.
      if (predictorEnabled) {
        // We also create a new predictor and register it with the new run.
        val p1 = predFactory.getNewForecasterRun(id)
        r1.register(p1)
        val np = p1 :: predictorRuns(id)
        predictorRuns = predictorRuns.updated(id, np)
      }
      r1
    }
  }

  /**
    * Determines whether expired runs should be collected for recycling.
    *
    * @param currentTimestamp The current timestamp.
    * @param lastCollectTime The timestamp when the last collection took place.
    * @return True if it is now time to recycle expired runs.
    */
  private def shouldCollect(
                             currentTimestamp: Long,
                             lastCollectTime: Long
                           ): Boolean = {
    val timeElapsedSinceLastCollect = currentTimestamp - lastCollectTime
    expirationDeadline != -1 & timeElapsedSinceLastCollect > expirationDeadline
  }

  /**
    * Cleans up pool of runs and DB connections and estimates statistics by retrieving info from predictorRuns.
    *
    * @return A profiler with the collected statistics.
    */
  private def shutdown(): WtProfiler = {
    logger.info("Shutting down...")
    val (locked, unlocked) = runPool.getSize
    val collectors = predictorRuns.map(p => (p._1, p._2.map(pr => pr.getCollector)))
    val profiler = WtProfiler()
    profiler.setGlobal(
      streamSize   = streamSize,
      execTime     = execTime,
      matchesNo    = matchesNo,
      lockedRuns   = locked,
      unlockedRuns = unlocked,
      matchDump    = md
    )
    if (predictorEnabled) profiler.createEstimators(collectors)
    profiler.estimateStats()
    runPool.shutdown()
    if (ConfigUtils.write2db) DBConnector.shutdown()
    logger.info("done.")
    profiler
  }

  /**
    * @return The list of FSMs.
    */
  def getFSMs: List[FSMInterface] = fsmList

  /**
    * @return The profiler. Should be called after shutdown, otherwise the profiler will be empty.
    */
  def getProfiler: WtProfiler = profiler

}
