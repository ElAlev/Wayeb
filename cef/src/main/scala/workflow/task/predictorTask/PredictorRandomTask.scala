package workflow.task.predictorTask

import com.typesafe.scalalogging.LazyLogging
import model.forecaster.RandomInterface
import model.forecaster.random.RandomForecaster
import ui.ConfigUtils
import workflow.provider.FSMProvider
import workflow.task.Task

object PredictorRandomTask {

  /**
    * Constructor for random predictor task.
    *
    * @param fsmp The provider for the FSMs.
    * @param horizon The horizon.
    * @return A random predictor task.
    */
  def apply(
             fsmp: FSMProvider,
             horizon: Int
           ): PredictorRandomTask = new PredictorRandomTask(fsmp, horizon)

}

/**
  * Creates random predictors.
  *
  * @param fsmp The provider for the FSMs.
  * @param horizon The horizon.
  */
class PredictorRandomTask private (
                                    fsmp: FSMProvider,
                                    horizon: Int
                                  ) extends Task with LazyLogging {

  private val randPredSeed = ConfigUtils.randPredSeed

  /**
    * Executes the task.
    *
    * @return A list of random predictor interfaces, one for each FSM.
    */
  override def execute(): List[RandomInterface] = {
    logger.info("Executing random predictor task...")
    val fsmList = fsmp.provide()
    val randpp = fsmList.map(fsm => (fsm.getId, fsm.getNonFinals, RandomForecaster(randPredSeed, horizon)))
    val ris = randpp.map(r => RandomInterface(r._1, r._3, r._2))
    logger.info("done.")
    ris
  }

}
