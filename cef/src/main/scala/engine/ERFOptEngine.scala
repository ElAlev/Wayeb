package engine

import com.typesafe.scalalogging.LazyLogging
import fsm.runtime._
import fsm.symbolic.sra.Configuration
import fsm.{NSRAInterface, SNFAInterface}
import profiler.WtProfiler
import stream.GenericEvent
import stream.source.{EndOfStreamEvent, StreamListener}
import utils.MathUtils
import workflow.provider.FSMProvider

object ERFOptEngine {
  /**
   * Constructor for engine.
   *
   * @param fsmProvider      The provider for the FSM(s) that run recognition.
   * @param show             Determines whether complex event matches and forecasts are to be displayed or not.
   * @param postProcess      Determines whether complex event matches will get a minimal processing after being detected.
   *                         Useful to ensure that the simple evens comprising a complex one do get accessed and not just
   *                         ignored.
   * @param warmupFirst      Determines whether the engine should go through a warmup phase first.
   * @param warmupStreamSize The number of warmup events.
   * @param findWarmupLimit  Determines whether the engine should try to find the warmup limit. When it finds the limit,
   *                         the engine reports it and shuts down. Not to be used for actual throughput measurements.
   * @param batchLength      The size of each warmup batch.
   * @param measurements     The number of batches used to estimate the slope of the throughput line.
   * @param reset            Determines whether runs should be reset to their start state after a full match.
   * @param memoryTest       Determines whether memory measurements should also be taken.
   * @return An optimized ERF engine
   */
  def apply(
             fsmProvider: FSMProvider,
             show: Boolean,
             postProcess: Boolean,
             warmupFirst: Boolean,
             warmupStreamSize: Int,
             findWarmupLimit: Boolean,
             batchLength: Int,
             measurements: Int,
             reset: Boolean,
             memoryTest: Boolean
           ): ERFOptEngine = new ERFOptEngine(fsmProvider, show, postProcess, warmupFirst, warmupStreamSize, findWarmupLimit, batchLength, measurements, reset, memoryTest)
}

/**
 * This is the main engine that runs recognition on a stream.
 * The engine is a stream listener (stream.source.StreamListener) that receives events from
 * a stream source (stream.source.StreamSource).
 *
 * The engine runs only recognition (no forecasting).
 * No partition attributes in patterns.
 * Currently works only with a single pattern.
 * CAUTION: Do not use with multiple patterns.
 *
 * The engine is optimized to achieve higher throughput figures.
 *
 * Works with non-deterministic automata to ensure proper enumeration of full matches.
 * Either SNFAs (non-relational, to be preferred for higher throughput when no relations are required in patterns).
 * Or NSRAs (relational, a bit slower than SNFAs).
 *
 * Each run is no longer represented as a separate object of the Run class.
 * Instead, all runs are represented as simple lists:
 *  - of state IDs and matches in case of SNFAs
 *  - of configurations and matches in case of NSRAs.
 *
 * There is the option of running a warmup phase initially so that the throughput has first stabilised before starting
 * to take measurements.
 *
 * There is also the option of finding the warmup limit, i.e., the number of events that the engine needs to first
 * process before its throughput is stabilised. The engine processes batches of events. For each batch, it calculates
 * the throughput. Then, for a series of batches it tries to "draw" the line of throughput - time (i.e., batch number).
 * It then estimates the slope of the line. If this is small, this means that the throughput has indeed stabilised.
 *
 *
 * @param fsmProvider       The provider for the FSM(s) that run recognition.
 * @param show              Determines whether complex event matches and forecasts are to be displayed or not.
 * @param postProcess       Determines whether complex event matches will get a minimal processing after being detected.
 *                          Useful to ensure that the simple evens comprising a complex one do get accessed and not just
 *                          ignored.
 * @param warmupFirst       Determines whether the engine should go through a warmup phase first.
 * @param warmupStreamSize  The number of warmup events.
 * @param findWarmupLimit   Determines whether the engine should try to find the warmup limit. When it finds the limit,
 *                          the engine reports it and shuts down. Not to be used for actual throughput measurements.
 * @param batchLength       The size of each warmup batch.
 * @param measurements      The number of batches used to estimate the slope of the throughput line.
 * @param reset             Determines whether runs should be reset to their start state after a full match.
 * @param memoryTest        Determines whether memory measurements should also be taken.
 *
 */
class ERFOptEngine(
                    fsmProvider: FSMProvider,
                    show: Boolean,
                    postProcess: Boolean,
                    warmupFirst: Boolean,
                    warmupStreamSize: Int,
                    findWarmupLimit: Boolean,
                    batchLength: Int,
                    measurements: Int,
                    reset: Boolean,
                    memoryTest: Boolean
                  ) extends StreamListener with LazyLogging {
  require(!(warmupFirst & findWarmupLimit), "You can either try to find the warmup limit or run the engine with warmup")
  logger.info("Initializing engine...")

  private val fsmList = fsmProvider.provide()

  // Stats for estimating warmup limit
  private var eventCounterInBatch: Int = 0
  private var currentLatencyAccumulator: Long = 0
  private var throughputAcc: List[Long] = List.empty
  val indep: List[Double] = (1 to measurements).toList.map(x => x.toDouble)
  private val throughputEpsilon: Double = 0.001
  private var slope: Double = throughputEpsilon + 1

  // How frequently memory measurements should be taken.
  private final val memoryMeasurementFreq: Int = 10000

  // Recognition stats
  private var matchesNo = 0
  private var execTime: Long = 0
  private var totalStreamSize: Int = 0
  private var effectiveStreamSize: Int = 0
  private var firstTimestamp: Long = 0
  private var lastTimestamp: Long = 0
  private var totalRuns: Long = 0
  private var maxMemTotal: Long = 0
  private var avgMemTotal: Long = 0
  private var maxMemUsed: Long = 0
  private var avgMemUsed: Long = 0
  private var countMemMeas: Int = 0
  private var profiler = WtProfiler()


  // Each FSM has a single run processor, called a mono-run.
  private val monoRunSNFAPool: List[MonoRunSNFA] = fsmList.map(fsm => MonoRunSNFA(fsm, show, postProcess))
  // SNFA runs represented as a list of state IDs and matches.
  // CAUTION: currentStatesMatches has the runs of only a single pattern/fsm, although monoRunSNFAPool seems to contain
  // mono-runs for multiple FSMs. If we want the engine to be able to handle multiple FSMs, we should have a list of
  // currentStatesMatches. Each element of this list would contain all the matches of the corresponding mono-run.
  // Then, engine.ERFOptEngine.processEventWithMonoRunSNFA would also have to be modified accordingly.
  //TODO: make engine capable of handling multiple patterns/FSMs.
  private var currentStatesMatches: List[(Int,MatchList)] = monoRunSNFAPool.map(r => (r.fsm.getStartId,MatchList()))

  // Each FSM has a single run processor, called a mono-run.
  private val monoRunNSRAPool: List[MonoRunNSRA] = fsmList.map(fsm => MonoRunNSRA(fsm, show, postProcess))
  // Each run represented as a tuple of (Configuration,Match).
  // CAUTION: currentConfsMatches has the runs of only a single pattern/fsm. See above.
  private var currentConfsMatches: List[(Configuration,MatchList)] = monoRunNSRAPool.map(r => (Configuration(r.fsm.getStartId),MatchList()))

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
        if (totalStreamSize % 100000 == 0) logger.debug(event.toString)
        processEventOpt(event)
        if (memoryTest & totalStreamSize % memoryMeasurementFreq == 0) memoryMeasure()
        if (findWarmupLimit) {
          if (eventCounterInBatch == batchLength) {
            val throughput = (batchLength.toDouble / (currentLatencyAccumulator.toDouble / 1000000000)).toInt
            throughputAcc = throughput :: throughputAcc
            if (throughputAcc.size > measurements) throughputAcc = throughputAcc.dropRight(1)
            logger.info("\n\tCurrent throughput: " + throughput)
            logger.info("\n\tLast throughputs: " + throughputAcc)
            if (throughputAcc.size >= measurements) {
              val throughputAccMax = throughputAcc.max
              val throughputAccNorm = throughputAcc.map(x => x.toDouble / throughputAccMax)
              val result = MathUtils.slopeInterceptLinearLeastSquares(indep, throughputAccNorm.reverse.map(x => x.toDouble)) //MathUtils.slopeInterceptLinearLeastSquares(List(2,3,5,7,9),List(4,5,7,10,15))
              slope = result._1
              logger.info(result.toString())
            }
            if (Math.abs(slope) < throughputEpsilon) {
              logger.info("\n\nThroughput stabilised at " + totalStreamSize + " events with throughput: " +  throughput + "  !\n\n")
              profiler = shutdown()
              profiler.printProfileInfo()
              System.exit(1)
            }
            else {
              eventCounterInBatch = 1
              currentLatencyAccumulator = 0
            }
          }
          else {
            eventCounterInBatch += 1
          }
        }
      }
    }
  }

  /**
   * Processing of a single event.
   *
   * @param event The event to process.
   */
  private def processEventOpt(event: GenericEvent): Unit = {
    fsmList.foreach {
      case _: SNFAInterface => processEventWithMonoRunSNFA(event)
      case _: NSRAInterface => processEventWithMonoRunNSRA(event)
      case _ => throw new Exception("Optimized engine accepts only SNFAs or NSRAs.")
    }
  }

  /**
   * Processing of a single event by all mono-runs.
   *
   * @param event The event to process.
   */
  private def processEventWithMonoRunSNFA(event: GenericEvent): Unit = {
    val t1 = System.nanoTime()
    val result = monoRunSNFAPool.map(run => processEventWithMonoRunSNFA(event, run, totalStreamSize))
    // Measurements taken only of we are out of warmup.
    if (outOfWarmup) {
      effectiveStreamSize += 1
      val newMatchesNo = result.map(x => x._1).sum
      matchesNo += newMatchesNo
      if (reset & newMatchesNo > 0) resetMonoRunSNFA()
      val t2 = System.nanoTime()
      val thisLatency = (t2 - t1)
      currentLatencyAccumulator += thisLatency
      execTime += thisLatency
    }
  }

  /**
   * Processing of a single event by a mono-run.
   *
   * @param event         The event to process.
   * @param thisMonoRun   The mono-run.
   * @param eventCounter  The event index.
   * @return  The number of full matches/complex events and the processing time.
   */
  private def processEventWithMonoRunSNFA(
                                           event: GenericEvent,
                                           thisMonoRun: MonoRunSNFA,
                                           eventCounter: Long
                                         ): (Int, Long) = {
    val t1 = System.nanoTime()
    // We first update all active states. If multiple runs are in the same state, then we can find the next state (and
    // output) for all of these runs with a single calculation. No need to repeat it for each such run. We thus have all
    // active states, i.e., states which have been reached by a run. We find the next states for these states only once.
    thisMonoRun.updateActiveStates(event)
    var tmpStatesMatches = List.empty[(Int,MatchList)]
    var detected: Int = 0
    // Now process each run.
    // We repeatedly call makeAMoveArrayPrealloc and accumulate gradually all new runs in tmpStatesMatches.
    while (currentStatesMatches.nonEmpty) {
      val stateMatch = currentStatesMatches.head
      val result = thisMonoRun.moveRunArrayPrealloc(event, stateMatch._1, eventCounter, stateMatch._2, tmpStatesMatches)
      detected += result._1
      tmpStatesMatches = result._2
      currentStatesMatches = currentStatesMatches.tail
    }
    currentStatesMatches = tmpStatesMatches
    val noOfCompleteMatches = detected
    val t2 = System.nanoTime()
    (noOfCompleteMatches, t2 - t1)
  }

  /**
   * Processing of a single event by all mono-runs.
   *
   * @param event The event to process.
   */
  private def processEventWithMonoRunNSRA(event: GenericEvent): Unit = {
    val t1 = System.nanoTime()
    val result = monoRunNSRAPool.map(run => processEventWithMonoRunNSRA(event, run, totalStreamSize))
    // Measurements taken only of we are out of warmup.
    if (outOfWarmup) {
      effectiveStreamSize += 1
      val newMatchesNo = result.map(x => x._1).sum
      matchesNo += newMatchesNo
      if (reset & newMatchesNo > 0) resetMonoRunNSRA()
      val t2 = System.nanoTime()
      val thisLatency = (t2 - t1)
      currentLatencyAccumulator += thisLatency
      execTime += thisLatency
    }
  }

  /**
   * Processing of a single event by a mono-run.
   *
   * @param event        The event to process.
   * @param thisMonoRun  The mono-run.
   * @param eventCounter The event index.
   * @return The number of full matches/complex events and the processing time.
   */
  private def processEventWithMonoRunNSRA(
                                           event: GenericEvent,
                                           thisMonoRun: MonoRunNSRA,
                                           eventCounter: Long
                                         ): (Int, Long) = {
    val t1 = System.nanoTime()
    var tmpConfsMatches = List.empty[(Configuration,MatchList)]
    var detected: Int = 0
    // Process each run.
    // We repeatedly call makeAMoveArrayPrealloc and accumulate gradually all new runs in tmpStatesMatches.
    while (currentConfsMatches.nonEmpty) {
      val confMatch = currentConfsMatches.head
      val result = thisMonoRun.moveRunArrayPrealloc(event, confMatch._1, eventCounter, confMatch._2, tmpConfsMatches)
      detected += result._1
      tmpConfsMatches = result._2
      currentConfsMatches = currentConfsMatches.tail
    }
    currentConfsMatches = tmpConfsMatches
    totalRuns += currentConfsMatches.size
    val noOfCompleteMatches = detected
    val t2 = System.nanoTime()
    (noOfCompleteMatches, t2 - t1)
  }

  /**
   * Resets all SNFA mono-runs.
   */
  private def resetMonoRunSNFA(): Unit = {
    currentStatesMatches = monoRunSNFAPool.map(r => (r.fsm.getStartId, MatchList()))
    monoRunSNFAPool.foreach(r => r.reset())
  }

  /**
   * Resets all NSRA mono-runs.
   */
  private def resetMonoRunNSRA(): Unit = {
    currentConfsMatches = monoRunNSRAPool.map(r => (Configuration(r.fsm.getStartId), MatchList()))
    monoRunNSRAPool.foreach(r => r.reset())
  }

  /**
   * Takes a memory measurement.
   */
  private def memoryMeasure(): Unit = {
    System.gc()
    var total: Long = Runtime.getRuntime.totalMemory
    avgMemTotal += total
    if (total > maxMemTotal) maxMemTotal = total
    //System.gc()
    var used: Long = total - Runtime.getRuntime.freeMemory
    avgMemUsed += used
    if (used > maxMemUsed) maxMemUsed = used
    countMemMeas += 1
  }

  /**
   * Determines whether we are out of the/any warmup phase or not.
   * True if there was no requirement for warmup in the first place or if there was such a requirement and we have
   * already processed the warmup events.
   *
   * @return True if we are out of warmup.
   */
  private def outOfWarmup: Boolean = (!warmupFirst) | (warmupFirst & totalStreamSize > warmupStreamSize)

  /**
   * Estimates recognition statistics.
   *
   * @return A profiler with the estimated statistics.
   */
  private def shutdown(): WtProfiler = {
    logger.info("Shutting down...")
    val profiler = WtProfiler()
    profiler.setGlobal(
      streamSize = effectiveStreamSize,
      firstTimestamp = firstTimestamp,
      lastTimestamp = lastTimestamp,
      execTime = execTime,
      matchesNo = matchesNo,
      lockedRuns = 0,
      unlockedRuns = 0,
      matchDump = new MatchDump
    )
    if (memoryTest) profiler.setMemStats(maxMemTotal, avgMemTotal, maxMemUsed, avgMemUsed, countMemMeas)
    profiler.estimateStats()
    logger.info("done.")
    profiler
  }

  def getProfiler: WtProfiler = profiler

}
