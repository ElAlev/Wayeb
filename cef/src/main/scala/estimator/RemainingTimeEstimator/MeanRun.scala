package estimator.RemainingTimeEstimator

import com.typesafe.scalalogging.LazyLogging
import estimator.RunEstimatorEngine
import fsm.FSMInterface
import stream.source.StreamSource

object MeanRun {
  /**
    * Constructor for a Mean run.
    *
    * @param fsm The FSM for which we want to build a model.
    * @return The Mean run.
    */
  def apply(fsm: FSMInterface): MeanRun = new MeanRun(fsm)
}

/**
  * A MeanRun is responsible for running estimation of a remaining time model for a FSM.
  *
  * @param fsm The FSM for which we want to build a model.
  */
class MeanRun private (fsm: FSMInterface) extends LazyLogging {

  /**
    * Method called to actually run estimation. For the same FSM, it can be called multiple times for different
    * training streams, if required.
    *
    * @param streamSource Source for the training stream.
    * @return The estimator holding the remaining times.
    */
  def estimateMeans(streamSource: StreamSource): MeanEstimator = {
    val learner = MeanEstimator(fsm)
    run(learner, streamSource)
    learner.estimate()
    learner
  }

  private def run(
                   learner: MeanEstimator,
                   streamSource: StreamSource
                 ): MeanEstimator = {
    logger.info("Running Mean estimation...")
    val meane = RunEstimatorEngine(fsm, learner)
    streamSource.emitEventsToListener(meane)
    learner
  }

}
