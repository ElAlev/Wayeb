package workflow.task.estimatorTask

import com.typesafe.scalalogging.LazyLogging
import estimator.MatrixEstimator.MLERun
import model.markov.{MarkovChain, MarkovChainFactory}
import stream.source.StreamSource
import workflow.provider.FSMProvider
import workflow.task.Task

object MatrixMLETask {
  /**
    * Constructor for MLE task.
    *
    * @param fsmp The provider for the list of FSMs.
    * @param streamSource The source for the training stream. All FSMs use the same training stream.
    * @return A MLE task.
    */
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): MatrixMLETask = new MatrixMLETask(fsmp, streamSource)
}

/**
  * This task is responsible for running MLE estimation for a list of FSMs in order to estimate their Markov chains.
  *
  * @param fsmp The provider for the list of FSMs.
  * @param streamSource The source for the training stream. All FSMs use the same training stream.
  */
class MatrixMLETask private (
    fsmp: FSMProvider,
    streamSource: StreamSource
) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of Markov chains for the FSMs, along with the execution time (for all FSMs).
    */
  override def execute(): (List[MarkovChain], Long) = {
    logger.info("Executing matrix estimation task (MLE)...")
    val t1 = System.nanoTime()
    // first get all FMSs
    val fsmList = fsmp.provide()
    // for each FSM, create a MLERun that will estimate the matrix
    val mleRuns = fsmList.map(fsm => MLERun(fsm))
    // now estimate all matrices
    val learners = mleRuns.map(mleRun => mleRun.estimateMatrix(streamSource))
    val fl = fsmList.zip(learners)
    // create a MC object for every FSM
    val mcs = fl.map(thisfl => MarkovChainFactory.buildMC(thisfl._2.getTransitionMatrix, thisfl._2.state2RowMapping, thisfl._1.getFinalsNo))
    val t2 = System.nanoTime()
    //logger.info(learners.foldLeft("") { (acc,x) => acc + "\n\t *** Transition matrix learnt ***\n " + x.m2str})
    logger.info("done.")
    (mcs, t2 - t1)
  }

}
