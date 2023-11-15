package workflow.provider

import estimator.HMMEstimator.IsoHMM
import stream.source.StreamSource
import workflow.condition.Condition
import workflow.provider.source.hmm.{HMMSource, HMMSourceDirect, HMMSourceEstimator}
import workflow.task.estimatorTask.HMMTask

object HMMProvider {
  /**
    * Constructor for HMM provider.
    *
    * @param hmmSource The HMM source.
    * @return The HMM provider.
    */
  def apply(hmmSource: HMMSource): HMMProvider = new HMMProvider(hmmSource, List.empty)
}

/**
  * According to type of source for HMM, builds a provider in the form of a list of IsoHMMs.
  *
  * @param hmmSource The source for the HMM:
  *                  - HMMSourceDirect when an already existing HMM is available.
  *                    Mostly used for testing and running experiments.
  *                  - HMMSourceEstimator when the HMM needs to be estimated.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class HMMProvider private (
                            hmmSource: HMMSource,
                            conditions: List[Condition]
                          ) extends AbstractProvider(conditions) {
  checkConditions()

  /**
    * Calling this function actually initiates the construction of the HMMs.
    *
    * @return A list of HMMs with their isomorphisms.
    */
  override def provide(): List[IsoHMM] = {
    hmmSource match {
      case x: HMMSourceDirect => x.hmms
      case x: HMMSourceEstimator => estimateHMMs(x.fsmp, x.streamSource)
      case _ => throw new Error("Not valid HMMSource")
    }
  }

  /**
    * Actually creates and trains a HMM with the given training stream.
    *
    * @param fsmp The provider for the FSMs.
    * @param streamSource The source for the training stream.
    * @return A list of HMMs.
    */
  private def estimateHMMs(
                            fsmp: FSMProvider,
                            streamSource: StreamSource
                          ): List[IsoHMM] = {
    val hmmt = HMMTask(fsmp, streamSource)
    hmmt.execute()._1
  }

  /**
    * Checks all conditions.
    * TODO: do the checking in the super-class workflow.provider.AbstractProvider?
    *
    * @return True if all conditions satisfied.
    */
  def checkConditions(): Boolean = {
    val superchecks = super.check()
    if (superchecks.contains(false)) {
      throw new Error("Provider conditions unsatisfied")
      false
    } else true
  }
}
