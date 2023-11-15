package estimator.OrderEstimator

import estimator.MatrixEstimator.MLERun
import fsm.CountPolicy.CountPolicy
import stream.array.EventStream
import stream.source.{ArrayStreamSource, EmitMode, StreamSource}
import ui.ConfigUtils
import workflow.provider.{AbstractProvider, DFAProvider, FSMProvider, SDFAProvider}
import workflow.provider.source.dfa.DFASourceFromXML
import workflow.provider.source.sdfa.SDFASourceFromSRE

object CrossValEstimator {
  /**
    * Constructor for cross validation estimator.
    *
    * @param fsmType Type of FSM, classical or symbolic.
    * @param patternFile Path to pattern file.
    * @param declarations Path to declarations file.
    * @param countPolicy Counting policy.
    * @param maxM Maximum order to check.
    * @param streamSource Source for stream from which the training and validation sets are extracted.
    * @return The estimator.
    */
  def apply(
             fsmType: String,
             patternFile: String,
             declarations: String,
             countPolicy: CountPolicy,
             maxM: Int,
             streamSource: StreamSource
           ): CrossValEstimator = new CrossValEstimator(
    fsmType,
    patternFile,
    declarations,
    countPolicy,
    maxM,
    streamSource
  )
}

/**
  * Estimator for the optimal order among full-order Markov models for a FSM.
  * For every order up to maxM, we learn a model using part of the stream as training set and then score on the rest
  * of the stream. We repeat for multiple folds. We finally estimate an average score from all folds. We end up with a
  * score for each order. We return the order with the best score.
  *
  * @param fsmType Type of FSM, classical or symbolic.
  * @param patternFile Path to pattern file.
  * @param declarations Path to declarations file.
  * @param countPolicy Counting policy.
  * @param maxM Maximum order to check.
  * @param streamSource Source for stream from which the training and validation sets are extracted.
  */
class CrossValEstimator private (
                                  fsmType: String,
                                  patternFile: String,
                                  declarations: String,
                                  countPolicy: CountPolicy,
                                  maxM: Int,
                                  streamSource: StreamSource
                                ) {
  private val numberOfFolds = ConfigUtils.defaultNumberOfFolds

  /**
    * For each order, find the score and then return the order with the best score.
    *
    * @return Order with the best score.
    */
  def estimateOrder(): Int = {
    var scores: List[Double] = Nil
    for (m <- 0 to maxM) {
      scores = runFolds(m, countPolicy, streamSource, numberOfFolds) :: scores
    }
    scores = scores.reverse
    val bestM = scores.indexOf(scores.min)
    bestM
  }

  /**
    * For a given order, create folds, estimate score for each fold and find the average score.
    *
    * @param order The order to test.
    * @param policy Counting policy.
    * @param streamSource Stream to be used for training and validation.
    * @param numberOfFolds The number of folds to create.
    * @return The average score for all folds.
    */
  private def runFolds(
                        order: Int,
                        policy: CountPolicy,
                        streamSource: StreamSource,
                        numberOfFolds: Int
                      ): Double = {
    val es = streamSource.emitEventsAndClose(EmitMode.BUFFER)
    val keepRatio = 0.1
    var totalScore = 0.0
    for (fold <- 0 until numberOfFolds) {
      // create training and validation sets by splitting the stream
      val (ts, vs) = es.split(keepRatio, fold)
      val foldScore = runFold(order, policy, ts, vs)
      totalScore += foldScore
    }
    val avgScore: Double = totalScore / numberOfFolds
    avgScore
  }

  /**
    * For a given order and fold, estimate the score.
    *
    * @param order The order to be tested.
    * @param policy Counting policy.
    * @param trainStream The training dataset for the fold.
    * @param validationStream The validation dataset for the fold.
    * @return The score for this fold.
    */
  private def runFold(
                       order: Int,
                       policy: CountPolicy,
                       trainStream: EventStream,
                       validationStream: EventStream
                     ): Double = {
    val fap: AbstractProvider = fsmType match {
      case "classical" => DFAProvider(DFASourceFromXML(patternFile, countPolicy, order, trainStream.getEventTypes))
      case "symbolic" => SDFAProvider(SDFASourceFromSRE(patternFile, policy, declarations))
      case _ => throw new IllegalArgumentException
    }
    val fsmp = FSMProvider(fap)
    val fsm = fsmp.provide().head // only the first pattern kept
    val mleRun = MLERun(fsm)
    val trainStreamSource = ArrayStreamSource(trainStream)
    val validationStreamSource = ArrayStreamSource(validationStream)
    mleRun.runScoreFold(trainStreamSource, validationStreamSource)
  }
}
