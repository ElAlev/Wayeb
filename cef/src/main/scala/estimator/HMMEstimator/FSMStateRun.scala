package estimator.HMMEstimator

import com.typesafe.scalalogging.LazyLogging
import estimator.RunEstimatorEngine
import fsm.{FSMInterface, SDFAInterface}
import stream.source.StreamSource

object FSMStateRun {
  /**
    * Constructor for FSM state run.
    *
    * @param fsm The FSM for which a HMM is to be trained.
    * @return The run.
    */
  def apply(fsm: FSMInterface): FSMStateRun =
    fsm match {
      case _: SDFAInterface => new FSMStateRun(fsm.asInstanceOf[SDFAInterface])
      case _ => throw new IllegalArgumentException("FSMStateRun accepts only SDFAInterface")
    }

}

/**
  * A FSMStateRun is responsible for running estimation of a HMM for a FSM.
  *
  * @param fsm The FSM for which a HMM is to be trained.
  */
class FSMStateRun private (fsm: SDFAInterface) extends LazyLogging {

  /**
    * Method called to actually run estimation. For the same FSM, it can be called multiple times for different
    * training streams, if required.
    *
    * @param streamSource Source for training stream.
    * @return The estimator holding the HMMs.
    */
  def estimateHMM(streamSource: StreamSource): FSMStateEstimator = {
    val learner = FSMStateEstimator(fsm)
    run(learner, streamSource)
    learner.estimate()
    learner
  }

  private def run(
                   learner: FSMStateEstimator,
                   streamSource: StreamSource
                 ): FSMStateEstimator = {
    logger.info("Running HMM estimation...")
    val hmme = RunEstimatorEngine(fsm, learner)
    streamSource.emitEventsToListener(hmme)
    learner
  }
}
