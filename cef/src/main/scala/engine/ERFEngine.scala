package engine

import profiler.WtProfiler
import stream.{GenericEvent, ResetEvent}
import com.typesafe.scalalogging.LazyLogging
import db.DBConnector
import fsm.{FSMInterface, NSRAInterface, SNFAInterface}
import fsm.runtime.{MatchDump, Run, RunPool}
import model.forecaster.runtime.{ForecasterRun, ForecasterRunFactory}
import stream.array.EventStream
import stream.source.{EndOfStreamEvent, StreamListener}
import ui.ConfigUtils
import utils.Progressor
import workflow.provider.{FSMProvider, ForecasterProvider}
import scala.collection.mutable

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
          show = ConfigUtils.defaultShowMatchesForecasts,
          reset = false
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
             show: Boolean,
             reset: Boolean
           ): ERFEngine = {
    new ERFEngine(fsmp,
                  predProvider,
                  predictorEnabled,
                  expirationDeadline,
                  collectStats,
                  finalsEnabled,
                  distance,
                  show,
                  reset
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
                          show: Boolean,
                          reset: Boolean
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
  private var predictorRuns: Map[Int, mutable.Map[Int,ForecasterRun]] = fsmList.map(fsm => fsm.getId).
    map(id => id -> mutable.Map.empty[Int,ForecasterRun]).toMap

  private var matchesNo = 0
  private var execTime: Long = 0
  private var lastCollectTime: Long = 0
  private var effectiveStreamSize: Int = 0
  private var firstTimestamp: Long = 0
  private var lastTimestamp: Long = 0
  private var profiler = WtProfiler()

  private var findRunsCalls: Int = 0
  private var findRunsTime: Long = 0

  private val warmupStreamSize: Int = 0
  private var totalStreamSize: Int = 0

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
        totalStreamSize += 1
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
    //logger.debug("PROCESSING " + event.toString)
    val det = fsmList.map(f => processEvent(event, f))
    if (totalStreamSize > warmupStreamSize) {
      effectiveStreamSize += 1
      matchesNo += det.map(d => d._1).sum
      //logger.debug("\n\tMatches thus far: " + matchesNo)
      val ts: Long = event.getValueOf("Timestamp").toString.toLong
      if (effectiveStreamSize == 1) firstTimestamp = ts
      lastTimestamp = ts
      // If it's time to collect stale runs, do so.
      if (shouldCollect(currentTimestamp, lastCollectTime)) {
        runPool.runsCollect(currentTimestamp)
       lastCollectTime = currentTimestamp
      }
      //execTime += det.map(d => d._2).sum
      val t2 = System.nanoTime()
      execTime += (t2 - t1)
    }
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
    effectiveStreamSize = eventStream.getSize
    logger.info("Running Wayeb for " + effectiveStreamSize + " events")
    val progressor = Progressor("ERFEngine", effectiveStreamSize, 5)
    for (i <- 0 until effectiveStreamSize) {
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
    * @return Number of CE detections. Also process time for runs in nanoseconds.
    */
  private def processEvent(
                            event: GenericEvent,
                            thisFsm: FSMInterface
                          ): (Int, Long) = {
    /**
      * First find the appropriate run(s).
      * Exactly one run per FSM is possible (and all FSMs attempt to process the event) for deterministic FSM.
      * For non-deterministic FSM, multiple runs may need to be considered.
      */
    val partitionAttribute = thisFsm.getPartitionAttribute
    // First retrieve the value of the  partition attribute,
    // i.e., the attribute by which the input stream is to be partitioned.
    val av = if (partitionAttribute.equalsIgnoreCase(singlePartitionVal)) singlePartitionVal
    else event.getValueOf(partitionAttribute).toString
    val runs = findRuns(event, thisFsm)
    var detected: Int = 0
    var processingTime: Long = 0
    thisFsm match {
      case _: SNFAInterface => {
        val results = runs.map(r => processEventAtRunNonDet(event, r, av))
        detected = results.map(_._1).sum
        processingTime = results.map(_._2).sum
      }
      case _: NSRAInterface => {
        val results = runs.map(r => processEventAtRunNonDet(event, r, av))
        detected = results.map(_._1).sum
        processingTime = results.map(_._2).sum
      }
      case _ => {
        processingTime = runs.map(r => r.processEventDet(event)).sum
        detected = runs.map(r => r.ceDetected).count(_ == true)
      }
    }
    if (reset & detected > 0) resetRunsForFSM(thisFsm.getId)
    (detected, processingTime)
  }

  /**
    * Processes an event with a run of a non-deterministic FSM.
    *
    * @param event The new event to be processed.
    * @param run The (non-deterministic) run.
    * @param attributeVal The value of the partition attribute.
    * @return The number of detected complex events and the time it took for the run to process the new event.
    */
  private def processEventAtRunNonDet(
                                       event: GenericEvent,
                                       run: Run,
                                       attributeVal: String
                                     ): (Int, Long) = {
    val t1 = System.nanoTime()
    var detected: Int = 0
    event match {
      case _: ResetEvent => killRun(run ,attributeVal)
      case _ => {
        // First check whether the run is ready to process the event.
        // For SPST(m) we might need to wait for a couple of events before we can start processing.
        // Other FSM types should always be ready.
        val ready = run.checkForReadiness(event)
        if (ready) {
          // find the next configurations
          val nextConfs = run.findNextConfigurations(event).toList
          if (nextConfs.nonEmpty) {
            // If there are more than one next configurations, we need to clone the run.
            // If there are k next configurations, we need to create k-1 clones (from the tail), so that we have a total
            // of k runs.
            val newRuns = nextConfs.tail.map(conf => runPool.checkOut(run, attributeVal, event.timestamp, conf))
            // OPT: No actual need to register the match dump with the run.
            // Sub-optimal. Comment out this registering for better performance.
            newRuns.foreach(r => registerRun(r, r.getFsmId))
            val allRuns = run :: newRuns
            //logger.debug("ACTIVE RUNS: " + allRuns.size)
            val statesAndRuns = nextConfs.zip(allRuns)
            // For all of the k runs, now try to produce forecasts (has an effect only in forecasting)
            statesAndRuns.foreach(sr => {
              val run = sr._2
              run.emitForecasts(event, sr._1)
            })
            detected = allRuns.count(r => r.ceDetected)
          }
          else {
            // if no next configurations have been found, this means that the run has nowhere to go,
            // therefore we need to kill it
            killRun(run, attributeVal)
          }
        }
      }
    }
    val t2 = System.nanoTime()
    val pt: Long = t2 - t1
    (detected, pt)
  }

  /**
    * Kills a given run.
    *
    * @param run The given run.
    * @param attributeVal The value of the partition attribute of the run.
    */
  private def killRun(
                       run: Run,
                       attributeVal: String
                     ): Unit = {
    runPool.release(run.fsm.getId, attributeVal, run.id)
    predictorRuns(run.fsm.getId).remove(run.id)
  }

  private def resetRunsForFSM(fsmId: Int): Unit = {
    runPool.reset(fsmId)
    predictorRuns = predictorRuns.updated(fsmId, mutable.Map.empty[Int, ForecasterRun])
  }

  /**
    * Method that finds the appropriate runs of an FSM that should do the actual processing of the event.
    *
    * @param event The event to be processed.
    * @param thisFsm The FSM for which we are trying to find an appropriate run.
    * @return The appropriate runs.
    */
  private def findRuns(
                        event: GenericEvent,
                        thisFsm: FSMInterface
                      ): List[Run] = {
    //return myRun
    val fsmId = thisFsm.getId
    val partitionAttribute = thisFsm.getPartitionAttribute
    // First retrieve the value of the  partition attribute,
    // i.e., the attribute by which the input stream is to be partitioned.
    val av = if (partitionAttribute.equalsIgnoreCase(singlePartitionVal)) singlePartitionVal
    else event.getValueOf(partitionAttribute).toString
    // If the pool of runs already has a run with this partition value (for the ID of the FSM we
    // are examining), then get this run.
    val foundRuns = if (runPool.existsRunWithAttributeVal(fsmId, av)) {
      findRunsCalls += 1
      val t1 = System.nanoTime()
      val runs = runPool.getRunsByAttribute(fsmId, av)
      val t2 = System.nanoTime()
      findRunsTime += (t2 - t1)
      runs
    } // Otherwise, create a new run with this partition value.
    else {
      val r1 = runPool.checkOut(fsmId, av, event.timestamp)
      // OPT: No actual need to register the match dump with the run.
      // Sub-optimal. Comment out this registering for better performance.
      registerRun(r1, fsmId)
      List(r1)
    }

    foundRuns
  }

  /**
    * Registers the match dump with a recognition run.
    * Also creates a new predictor run (if forecasting is enabled) and registers it with the recognition.
    *
    * @param run The given recognition run.
    * @param fsmId The FSM id of the run.
    */
  private def registerRun(
                           run: Run,
                           fsmId: Int
                         ): Unit = {
    run.register(md) // Register the MatchDump with the new run.
    if (predictorEnabled) {
      // We also create a new predictor and register it with the new run.
      val p1 = predFactory.getNewForecasterRun(fsmId, run.id)
      run.register(p1)
      val np = predictorRuns(fsmId) + (run.id -> p1)//p1 :: predictorRuns(fsmId)
      predictorRuns = predictorRuns.updated(fsmId, np)
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
    val collectors = predictorRuns.map(p => (p._1, p._2.values.map(pr => pr.getCollector).toList))
    val profiler = WtProfiler()
    profiler.setGlobal(
      streamSize   = effectiveStreamSize,
      firstTimestamp = firstTimestamp,
      lastTimestamp = lastTimestamp,
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
