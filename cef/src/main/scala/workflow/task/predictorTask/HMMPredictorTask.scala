package workflow.task.predictorTask

import com.typesafe.scalalogging.LazyLogging
import model.forecaster.HMMInterface
import model.waitingTime.ForecastMethod.ForecastMethod
import workflow.provider.{FSMProvider, HMMProvider}
import workflow.task.Task

object HMMPredictorTask {
  /**
    * Constructor for HMM predictor task.
    *
    * @param fsmp The provider for the FSMs.
    * @param hmmp The provider for the HMMs.
    * @param horizon The horizon.
    * @param predictionThreshold The confidence threshold (same for all states and all FSMs).
    * @param maxSpread The maximum spread (same for all states and all FSMs).
    * @param method The forecasting method, see model.waitingTime.ForecastMethod (same for all states and all FSMs).
    * @return A HMM predictor task.
    */
  def apply(
             fsmp: FSMProvider,
             hmmp: HMMProvider,
             horizon: Int,
             predictionThreshold: Double,
             maxSpread: Int,
             method: ForecastMethod
           ): HMMPredictorTask = new HMMPredictorTask(fsmp, hmmp, horizon, predictionThreshold, maxSpread, method)
}

/**
  * Creates HMM predictor interfaces for a list of FSMs.
  *
  * @param fsmp The provider for the FSMs.
  * @param hmmp The provider for the HMMs.
  * @param horizon The horizon.
  * @param predictionThreshold The confidence threshold (same for all states and all FSMs).
  * @param maxSpread The maximum spread (same for all states and all FSMs).
  * @param method The forecasting method, see model.waitingTime.ForecastMethod (same for all states and all FSMs).
  */
class HMMPredictorTask private (
                                 fsmp: FSMProvider,
                                 hmmp: HMMProvider,
                                 horizon: Int,
                                 predictionThreshold: Double,
                                 maxSpread: Int,
                                 method: ForecastMethod
                               ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of HMM predictor interfaces, one for each FSM.
    */
  override def execute(): (List[HMMInterface], Long) = {
    logger.info("Executing hmm predictor task...")
    val t1 = System.nanoTime()
    val fsmList = fsmp.provide()
    val hmmList = hmmp.provide()
    val fmList = fsmList.zip(hmmList)
    val hmmis = fmList.map(fm => HMMInterface(fm._1.getId, method, predictionThreshold, maxSpread, horizon, fm._2, fm._1))
    val t2 = System.nanoTime()
    logger.info("done.")
    (hmmis, t2 - t1)
  }

}
