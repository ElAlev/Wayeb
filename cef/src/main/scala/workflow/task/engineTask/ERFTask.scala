package workflow.task.engineTask

import com.typesafe.scalalogging.LazyLogging
import engine.ERFEngine
import profiler.WtProfiler
import stream.source.StreamSource
import ui.ConfigUtils
import workflow.provider.source.forecaster.ForecasterSourceRandom
import workflow.provider.{FSMProvider, ForecasterProvider}
import workflow.task.Task

object ERFTask {
  /**
    * Constructor for ERF task.
    *
    * @param fsmp The provider for the FSM.
    * @param pp The provider for the predictor.
    * @param predictorEnabled If true, forecasting is enabled. If false, only recognition.
    * @param finalsEnabled If true, forecasts from final states are emitted (the distances provided must also include the
    *                      finals).
    * @param expirationDeadline The time a FSM run is allowed to live before being recycled. See engine.ERFEngine.
    * @param distance The distances to determine when forecasts are allowed to be emitted. See engine.ERFEngine.
    * @param streamSource The source for the event stream.
    * @param collectStats If true, recognition and forecasting statistics are enabled (lowers performance).
    * @param show Determines whether complex event matches and forecasts are to be displayed or not.
    * @return An ERT task.
    */
  def apply(
             fsmp: FSMProvider,
             pp: ForecasterProvider,
             predictorEnabled: Boolean,
             finalsEnabled: Boolean,
             expirationDeadline: Long,
             distance: (Double, Double),
             streamSource: StreamSource,
             collectStats: Boolean,
             show: Boolean
           ): ERFTask = new ERFTask(
    fsmp,
    pp,
    predictorEnabled,
    finalsEnabled,
    expirationDeadline,
    distance,
    streamSource,
    collectStats,
    show
  )

  /**
    * Constructor for ERF task.
    * Defaults values for predictorEnabled, expirationDeadline, distance, collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param pp The provider for the predictor.
    * @param finalsEnabled If true, forecasts from final states are emitted (the distances provided must also include the
    *                      finals).
    * @param streamSource The source for the event stream.
    * @return An ERF task.
    */
  def apply(
             fsmp: FSMProvider,
             pp: ForecasterProvider,
             finalsEnabled: Boolean,
             streamSource: StreamSource
           ): ERFTask = new ERFTask(
    fsmp,
    pp,
    predictorEnabled = ConfigUtils.defaultPredictorEnabled,
    finalsEnabled,
    expirationDeadline = ConfigUtils.defaultExpiration,
    distance       = ConfigUtils.defaultDistance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show = ConfigUtils.defaultShowMatchesForecasts
  )

  /**
    * Constructor for ERF task.
    * Defaults values for predictorEnabled, finalsEnabled, expirationDeadline, distance, collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param pp The provider for the predictor.
    * @param streamSource The source for the event stream.
    * @param show Determines whether complex event matches and forecasts are to be displayed or not.
    * @return An ERF task.
    */
  def apply(
             fsmp: FSMProvider,
             pp: ForecasterProvider,
             streamSource: StreamSource,
             show: Boolean
           ): ERFTask = new ERFTask(
    fsmp,
    pp,
    predictorEnabled = ConfigUtils.defaultPredictorEnabled,
    finalsEnabled    = ConfigUtils.defaultFinalsEnabled,
    expirationDeadline   = ConfigUtils.defaultExpiration,
    distance         = ConfigUtils.defaultDistance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show
  )

  /**
    * Constructor for ERF task.
    * Defaults values for predictorEnabled, finalsEnabled, expirationDeadline, collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param pp The provider for the predictor.
    * @param distance The distances to determine when forecasts are allowed to be emitted. See engine.ERFEngine.
    * @param streamSource The source for the event stream.
    * @return An ERF task.
    */
  def apply(
             fsmp: FSMProvider,
             pp: ForecasterProvider,
             distance: (Double, Double),
             streamSource: StreamSource
           ): ERFTask = new ERFTask(
    fsmp,
    pp,
    predictorEnabled = ConfigUtils.defaultPredictorEnabled,
    finalsEnabled    = ConfigUtils.defaultFinalsEnabled,
    expirationDeadline   = ConfigUtils.defaultExpiration,
    distance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show = ConfigUtils.defaultShowMatchesForecasts
  )

  /**
    * Constructor for ERF task.
    * Default value for collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param pp The provider for the predictor.
    * @param predictorEnabled If true, forecasting is enabled. If false, only recognition.
    * @param finalsEnabled If true, forecasts from final states are emitted (the distances provided must also include the
    *                      finals).
    * @param expirationDeadline The time a FSM run is allowed to live before being recycled. See engine.ERFEngine.
    * @param distance The distances to determine when forecasts are allowed to be emitted. See engine.ERFEngine.
    * @param streamSource The source for the event stream.
    * @return An ERT task.
    */
  def apply(
             fsmp: FSMProvider,
             pp: ForecasterProvider,
             predictorEnabled: Boolean,
             finalsEnabled: Boolean,
             expirationDeadline: Long,
             distance: (Double, Double),
             streamSource: StreamSource
           ): ERFTask = new ERFTask(
    fsmp,
    pp,
    predictorEnabled,
    finalsEnabled,
    expirationDeadline,
    distance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show = ConfigUtils.defaultShowMatchesForecasts
  )


  /**
    * Constructor for ERF task. To be used for recognition. Forecasting is disabled.
    * Default value for expirationDeadline, collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param streamSource The source for the event stream.
    * @return
    */
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): ERFTask = new ERFTask(
    fsmp,
    ForecasterProvider(new ForecasterSourceRandom(fsmp, 1)),
    predictorEnabled = false,
    finalsEnabled = false,
    expirationDeadline =ConfigUtils.defaultExpiration,
    distance = ConfigUtils.defaultDistance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show = ConfigUtils.defaultShowMatchesForecasts
  )

  /**
    * Constructor for ERF task. To be used for recognition. Forecasting is disabled.
    * Default value for expirationDeadline, collectStats.
    *
    * @param fsmp The provider for the FSM.
    * @param streamSource The source for the event stream.
    * @param show Determines whether complex event matches and forecasts are to be displayed or not.
    * @return
    */
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource,
             show: Boolean
           ): ERFTask = new ERFTask(
    fsmp,
    ForecasterProvider(new ForecasterSourceRandom(fsmp, 1)),
    predictorEnabled = false,
    finalsEnabled = false,
    expirationDeadline =ConfigUtils.defaultExpiration,
    distance = ConfigUtils.defaultDistance,
    streamSource,
    collectStats = ConfigUtils.defaultCollectStats,
    show
  )
}

/**
  * Task that runs event recognition and forecasting.
  *
  * @param fsmp The provider for the FSM.
  * @param pp The provider for the predictor.
  * @param predictorEnabled If true, forecasting is enabled. If false, only recognition.
  * @param finalsEnabled If true, forecasts from final states are emitted (the distances provided must also include the
  *                      finals).
  * @param expirationDeadline The time a FSM run is allowed to live before being recycled. See engine.ERFEngine.
  * @param distance The distances to determine when forecasts are allowed to be emitted. See engine.ERFEngine.
  * @param streamSource The source for the event stream.
  * @param collectStats If true, recognition and forecasting statistics are enabled (lowers performance).
  * @param show Determines whether complex event matches and forecasts are to be displayed or not.
  */
class ERFTask private (
                        fsmp: FSMProvider,
                        pp: ForecasterProvider,
                        predictorEnabled: Boolean,
                        finalsEnabled: Boolean,
                        expirationDeadline: Long,
                        distance: (Double, Double),
                        streamSource: StreamSource,
                        collectStats: Boolean,
                        show: Boolean
                      ) extends Task with LazyLogging {

  private val engine =
    ERFEngine(fsmp, pp, predictorEnabled, expirationDeadline, collectStats, finalsEnabled, distance, show)

  /**
    * Sends events to the engine.
    *
    * @return A profiler with statistics.
    */
  override def execute(): WtProfiler = {
    logger.info("Executing ERF task...")
    streamSource.emitEventsToListener(engine)
    val profiler = engine.getProfiler
    logger.info("done.")
    profiler
  }

  def getEngine: ERFEngine = engine

}
