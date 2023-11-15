package workflow.task.estimatorTask

import com.typesafe.scalalogging.LazyLogging
import estimator.HMMEstimator.{FSMStateRun, IsoHMM}
import stream.source.StreamSource
import workflow.provider.FSMProvider
import workflow.task.Task

object HMMTask {
  /**
    * Constructor for HMM task.
    *
    * @param fsmp The provider for the list of FSMs.
    * @param streamSource Source for training stream.
    * @return A HMM task.
    */
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): HMMTask = new HMMTask(fsmp, streamSource)
}

/**
  * This task is responsible for building HMMs for a list of FSMs.
  *
  * @param fsmp The provider for the list of FSMs.
  * @param streamSource Source for training stream.
  */
class HMMTask private (
                        fsmp: FSMProvider,
                        streamSource: StreamSource
) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list with HMMs, one for each FSM, along with the time taken to run the task. The IsoHMM class,
    *         besides the HMM itself, also contains mappers. See estimator.HMMEstimator.FSMStateEstimator for an
    *         explanation.
    */
  override def execute(): (List[IsoHMM], Long) = {
    logger.info("Executing HMM task ...")
    val t1 = System.nanoTime()
    // first get all FMSs
    val fsmList = fsmp.provide()
    // for each FSM, create a FSMStateRun that will estimate the HMM
    val fsmStateRuns = fsmList.map(fsm => FSMStateRun(fsm))
    // now estimate the HMMs
    val learners = fsmStateRuns.map(stateRun => stateRun.estimateHMM(streamSource))
    val fl = fsmList.zip(learners)
    val rts = fl.map(thisfl => thisfl._2.getHMM)
    val t2 = System.nanoTime()
    logger.info(rts.foldLeft("") { (acc, x) => acc + "\n\t *** HMMs learnt ***\n " + x.toString })
    logger.info("done.")
    (rts, t2 - t1)
  }
}
