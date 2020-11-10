package workflow.task.predictorTask

import com.typesafe.scalalogging.LazyLogging
import model.waitingTime.ForecastMethod.ForecastMethod
import model.forecaster.WtInterface
import model.forecaster.wt.WtForecasterBuilder
import workflow.provider.{FSMProvider, WtProvider}
import workflow.task.Task

object WtPredictorTask {
  /**
    * Constructor for waiting-time predictor task.
    *
    * @param fsmp The provider for the FSMs.
    * @param wtdp The provider for the waiting-time distributions.
    * @param horizon The horizon.
    * @param predictionThreshold The confidence threshold (same for all states and all FSMs).
    * @param maxSpread The maximum spread (same for all states and all FSMs).
    * @param method The forecasting method, see model.waitingTime.ForecastMethod (same for all states and all FSMs).
    * @return A waiting-time predictor task.
    */
  def apply(
             fsmp: FSMProvider,
             wtdp: WtProvider,
             horizon: Int,
             predictionThreshold: Double,
             maxSpread: Int,
             method: ForecastMethod
           ): WtPredictorTask = new WtPredictorTask(
    fsmp,
    wtdp,
    horizon,
    predictionThreshold,
    maxSpread,
    method
  )
}

/**
  * Creates waiting-time predictors.
  *
  * @param fsmp The provider for the FSMs.
  * @param wtdp The provider for the waiting-time distributions.
  * @param horizon The horizon.
  * @param predictionThreshold The confidence threshold (same for all states and all FSMs).
  * @param maxSpread The maximum spread (same for all states and all FSMs).
  * @param method The forecasting method, see model.waitingTime.ForecastMethod (same for all states and all FSMs).
  */
class WtPredictorTask private (
                                fsmp: FSMProvider,
                                wtdp: WtProvider,
                                horizon: Int,
                                predictionThreshold: Double,
                                maxSpread: Int,
                                method: ForecastMethod
                              ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of waiting-time predictor interfaces, one for each FSM.
    */
  override def execute(): (List[WtInterface], Long) = {
    logger.info("Executing waiting-time predictor task...")
    val t1 = System.nanoTime()
    val fsmList = fsmp.provide()
    val wtdList = wtdp.provide()
    val fmList = fsmList.zip(wtdList)
    val buildersList = fmList.map(fm => (fm._1.getId, WtForecasterBuilder(fm._1, fm._2, horizon, predictionThreshold, maxSpread, method)))
    val wtpp = buildersList.map(wtpb => (wtpb._1, wtpb._2.wtForecaster))
    val wtis = wtpp.map(wtp => WtInterface(wtp._1, wtp._2, maxSpread))
    val t2 = System.nanoTime()
    logger.info("done.")
    (wtis, t2 - t1)
  }

}
