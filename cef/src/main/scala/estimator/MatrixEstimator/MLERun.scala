package estimator.MatrixEstimator

import com.typesafe.scalalogging.LazyLogging
import estimator.RunEstimatorEngine
import fsm.FSMInterface
import stream.source.StreamSource

object MLERun {
  /**
    * Constructor for MLE run.
    *
    * @param fsm The FSM for which to run MLE.
    * @return The MLE run.
    */
  def apply(fsm: FSMInterface): MLERun = new MLERun(fsm)
}

/**
  * A MLERun is responsible for running maximum likelihood estimation for a FSM.
  *
  * @param fsm The FSM for which to run MLE.
  */
class MLERun private (fsm: FSMInterface) extends LazyLogging {

  /**
    * Method called to actually run estimation. For the same FSM, it can be called multiple times for different
    * training streams, if required.
    *
    * @param streamSource Source for the training stream
    * @return The estimator holding the transition matrix
    */
  def estimateMatrix(streamSource: StreamSource): MLEEstimator = {
    val learner = MLEEstimator(fsm)
    run(learner, streamSource)
    learner.estimate()
    learner
  }

  /**
    * Used for optimal order estimation. Learns a model from a training stream and then estimates the model's score
    * on a validation stream.
    *
    * @param trainStreamSource Source for the training stream.
    * @param validationStreamSource Source for the validation stream.
    * @return Score on the validation stream.
    */
  def runScoreFold(
                    trainStreamSource: StreamSource,
                    validationStreamSource: StreamSource
                  ): Double = {
    val learner = estimateMatrix(trainStreamSource)
    score(learner, validationStreamSource)
  }

  private def score(
                     learner: MLEEstimator,
                     streamSource: StreamSource
                   ): Double = {
    learner.buildRanking("modified")
    learner.cleanN()
    run(learner, streamSource)
    learner.score
  }

  private def run(
                   learner: MLEEstimator,
                   streamSource: StreamSource
                 ): MLEEstimator = {
    logger.info("Running MLE...")
    val mlee = RunEstimatorEngine(fsm, learner)
    streamSource.emitEventsToListener(mlee)
    learner
  }

}
