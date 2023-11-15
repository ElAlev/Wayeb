package workflow.task.engineTask

import com.typesafe.scalalogging.LazyLogging
import engine.ERFOptEngine
import profiler.WtProfiler
import stream.source.StreamSource
import workflow.provider.FSMProvider
import workflow.task.Task

object ERFOptTask {

  /**
   * Constructor for ERFOpt task.
   *
   * @param fsmp             The provider for the FSM(s) that run recognition.
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
   * @param streamSource     The source for the event stream.
   * @param reset            Determines whether runs should be reset to their start state after a full match.
   * @param timeout          The time (in seconds) the source will be allowed to run. After the timeout, the source
   *                         should stop emitting events.
   * @param memoryTest       Determines whether memory measurements should also be taken.
   *
   * @return An ERFOpt task.
   *
   */
  def apply(
             fsmp: FSMProvider,
             show: Boolean,
             postProcess: Boolean,
             warmupFirst: Boolean,
             warmupStreamSize: Int,
             findWarmupLimit: Boolean,
             batchLength: Int,
             measurements: Int,
             streamSource: StreamSource,
             reset: Boolean,
             timeout: Long,
             memoryTest: Boolean
           ): ERFOptTask = new ERFOptTask(fsmp, show, postProcess, warmupFirst, warmupStreamSize, findWarmupLimit, batchLength, measurements, streamSource, reset, timeout, memoryTest)
}

/**
 * Task that runs event recognition and forecasting.
 *
 * @param fsmp              The provider for the FSM(s) that run recognition.
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
 * @param streamSource      The source for the event stream.
 * @param reset             Determines whether runs should be reset to their start state after a full match.
 * @param timeout           The time (in seconds) the source will be allowed to run. After the timeout, the source
 *                          should stop emitting events.
 * @param memoryTest        Determines whether memory measurements should also be taken.
 */
class ERFOptTask private (
                           fsmp: FSMProvider,
                           show: Boolean,
                           postProcess: Boolean,
                           warmupFirst: Boolean,
                           warmupStreamSize: Int,
                           findWarmupLimit: Boolean,
                           batchLength: Int,
                           measurements: Int,
                           streamSource: StreamSource,
                           reset: Boolean,
                           timeout: Long,
                           memoryTest: Boolean
                         ) extends Task with LazyLogging{

  private val engine = ERFOptEngine(fsmp, show, postProcess, warmupFirst, warmupStreamSize, findWarmupLimit, batchLength, measurements, reset, memoryTest)

  override def execute(): WtProfiler = {
    logger.info("Executing ERF task...")
    streamSource.emitEventsToListener(engine, timeout)
    val profiler = engine.getProfiler
    logger.info("done.")
    profiler
  }
}
