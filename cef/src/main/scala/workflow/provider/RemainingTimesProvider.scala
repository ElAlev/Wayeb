package workflow.provider

import estimator.RemainingTimeEstimator.RemainingTimes
import stream.source.StreamSource
import workflow.condition.Condition
import workflow.provider.source.rt.{RTSource, RTSourceDirect, RTSourceEstimator}
import workflow.task.estimatorTask.MeanTask

object RemainingTimesProvider {
  /**
    * Constructor for RT provider.
    *
    * @param rtSource The source for RT:
    *                 - RTSourceDirect when the RT already exists.
    *                 - RTSourceEstimator when remaining times must be estimated.
    * @return A RT provider.
    */
  def apply(rtSource: RTSource): RemainingTimesProvider = new RemainingTimesProvider(rtSource, List.empty[Condition])
}

/**
  * According to type of source for RT, builds a provider in the form of a list of remaining times.
  *
  * @param rtSource The source for RT:
  *                 - RTSourceDirect when the RT already exists.
  *                 - RTSourceEstimator when remaining times must be estimated.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class RemainingTimesProvider private (
                                       rtSource: RTSource,
                                       conditions: List[Condition]
                                     ) extends AbstractProvider(conditions) {

  /**
    * Calling this function actually initiates the construction of the remaining times.
    *
    * @return A list of RTs.
    */
  override def provide(): List[RemainingTimes] = {
    rtSource match {
      case x: RTSourceDirect => x.rts
      case x: RTSourceEstimator => estimateRTs(x.fsmp, x.streamSource)
      case _ => throw new Error("Not valid RTSource")
    }
  }

  /**
    * Estimates remaining times for a list of given FSM.
    *
    * @param fsmp The provider for the FSM.
    * @param streamSource The source for the training stream.
    * @return A list of RTs.
    */
  private def estimateRTs(
                           fsmp: FSMProvider,
                           streamSource: StreamSource
                         ): List[RemainingTimes] = {
    val mlet = MeanTask(fsmp, streamSource)
    mlet.execute()._1
  }

}
