package workflow.task.estimatorTask

import com.typesafe.scalalogging.LazyLogging
import estimator.RemainingTimeEstimator.{MeanRun, RemainingTimes}
import stream.source.StreamSource
import workflow.provider.FSMProvider
import workflow.task.Task

object MeanTask {
  /**
    * Constructor for mean task.
    *
    * @param fsmp The provider for the FSMs for which we want to build models.
    * @param streamSource The source for the training stream.
    * @return A mean task.
    */
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): MeanTask = new MeanTask(fsmp, streamSource)
}

/**
  * This task is responsible for building a prediction model based on the mean remaining times of a FSM.
  *
  * @param fsmp The provider for the FSMs for which we want to build models.
  * @param streamSource The source for the training stream.
  */
class MeanTask private (
                         fsmp: FSMProvider,
                         streamSource: StreamSource
                       ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list with remaining times models, one for each FSM, along with the time taken to run the task.
    */
  override def execute(): (List[RemainingTimes], Long) = {
    logger.info("Executing mean task ...")
    val t1 = System.nanoTime()
    // first get all FMSs
    val fsmList = fsmp.provide()
    // for each FSM, create a MeanRun that will estimate the model
    val meanRuns = fsmList.map(fsm => MeanRun(fsm))
    // now estimate the mean models
    val learners = meanRuns.map(meanRun => meanRun.estimateMeans(streamSource))
    val fl = fsmList.zip(learners)
    val rts = fl.map(thisfl => thisfl._2.getRemainingTimes)
    val t2 = System.nanoTime()
    logger.info(rts.foldLeft("") { (acc, x) => acc + "\n\t *** Mean remaining times learnt ***\n " + x.toString })
    logger.info("done.")
    (rts, t2 - t1)
  }

}
