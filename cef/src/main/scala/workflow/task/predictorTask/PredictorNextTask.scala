package workflow.task.predictorTask

import com.typesafe.scalalogging.LazyLogging
import model.forecaster.NextInterface
import model.forecaster.next.NextForecasterBuilder
import workflow.provider.{FSMProvider, MarkovChainProvider}
import workflow.task.Task

object PredictorNextTask {
  /**
    * Constructor for next predictor task.
    *
    * @param fsmp The provider for the FSMs.
    * @param mcp The provider for the Markov chains of the FSMs.
    * @return A next predictor task.
    */
  def apply(
             fsmp: FSMProvider,
             mcp: MarkovChainProvider
           ): PredictorNextTask = new PredictorNextTask(fsmp, mcp)
}

/**
  * Creates next predictors for a list of FSMs.
  *
  * @param fsmp The provider for the FSMs.
  * @param mcp The provider for the Markov chains of the FSMs.
  */
class PredictorNextTask private (
                                  fsmp: FSMProvider,
                                  mcp: MarkovChainProvider
                                ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of next predictor interfaces.
    */
  override def execute(): List[NextInterface] = {
    logger.info("Executing predictor (next) task...")
    val fsmList = fsmp.provide()
    val mcList = mcp.provide()
    val fmList = fsmList.zip(mcList)
    val buildersList = fmList.map(fm => (fm._1.getId, NextForecasterBuilder(fm._1, fm._2)))
    val npp = buildersList.map(npb => (npb._1, npb._2.createForecaster()))
    val nis = npp.map(np => NextInterface(np._1, np._2))
    logger.info("done.")
    nis
  }

}
