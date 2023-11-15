package workflow.provider

import fsm.{DSRAInterface, SDFAInterface}
import model.vmm.mapper.{Isomorphism, SymbolMapper}
import stream.source.StreamSource
import model.vmm.VMMUtils
import model.vmm.pst.{CounterSuffixTree, PredictionSuffixTree}
import workflow.provider.source.pst.{PSTSource, PSTSourceCST, PSTSourceDirect, PSTSourceLearnerFromDSRA, PSTSourceLearnerFromSDFA}
import workflow.condition.Condition

object PSTProvider {
  /**
    * Constructor for PST provider.
    *
    * @param pstSource The source for the PST:
    *                  - PSTSourceDirect when the PST already exists.
    *                  - PSTSourceLearner when the PST must be learnt.
    * @return          A PST provider.
    */
  def apply(pstSource: PSTSource): PSTProvider = new PSTProvider(pstSource, List.empty)
}

/**
  * According to type of source for PST, builds a provider in the form of a list of PSTs along with their respective
  * isomorphism.
  *
  * @param pstSource The source for the PST:
  *                  - PSTSourceDirect when the PST already exists.
  *                  - PSTSourceLearner when the PST must be learnt.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class PSTProvider private (
                            pstSource: PSTSource,
                            conditions: List[Condition]
                          ) extends AbstractProvider(conditions) {

  checkConditions()

  /**
    * Calling this function actually initiates the construction of the PSTs.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the file the the serialized PST) and then call
    * provide(). Keep the source until you call provide().
    *
    * @return A list of PSTs with their symbol mappers.
    */
  override def provide(): List[(PredictionSuffixTree, SymbolMapper)] = {
    pstSource match {
      case x: PSTSourceDirect => x.pst
      case x: PSTSourceLearnerFromSDFA => learnPSTs(x.sdfap, x.trainStream, x.maxOrder, x.pMin, x.alpha, x.gammaMin, x.r)
      case x: PSTSourceLearnerFromDSRA => learnPSTs(x.dsrap, x.trainStream, x.maxOrder, x.pMin, x.alpha, x.gammaMin, x.r)
      case x: PSTSourceCST => transformToPSTs(x.cstIsosOrder, x.pMin, x.alpha, x.gammaMin, x.r)
      case _ => throw new Error("Not valid PSTSource")
    }

  }

  /**
    * Learns PSTs from the given SDFA and a training stream.
    *
    * @param sdfap        The provider for the SDFA.
    * @param streamSource The source for the training stream (same for all PSTs).
    * @param maxOrder     The maximum order of the PST (same for all PSTs).
    * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded (same for all PSTs).
    * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold (same for all PSTs).
    * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold (same for all PSTs).
    * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the
    *                     conditional on the expanded context by the conditional on the original context is greater than
    *                     this threshold (same for all PSTs).
    * @return             A list of prediction suffix trees and their symbol mappers.
    */
  private def learnPSTs(
                         sdfap: SDFAProvider,
                         streamSource: StreamSource,
                         maxOrder: Int,
                         pMin: Double,
                         alpha: Double,
                         gammaMin: Double,
                         r: Double
                       ): List[(PredictionSuffixTree, SymbolMapper)] = {
    val sdfais = sdfap.provide()
    val pstWithSMs = sdfais.map(sdfai => learnPST(sdfai, streamSource, maxOrder, pMin, alpha, gammaMin, r))
    pstWithSMs
  }

  /**
   * Learns PSTs from the given dSRA and a training stream.
   *
   * @param dsrap        The provider for the dSRA.
   * @param streamSource The source for the training stream (same for all PSTs).
   * @param maxOrder     The maximum order of the PST (same for all PSTs).
   * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded (same for all PSTs).
   * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
   *                     context must be greater than this threshold (same for all PSTs).
   * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
   *                     expanded context must be greater than this threshold (same for all PSTs).
   * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
   *                     on the expanded context by the conditional on the original context is greater than this threshold (same
   *                     for all PSTs).
   * @return             A list of prediction suffix trees and their symbol mappers.
   */
  private def learnPSTs(
                         dsrap: DSRAProvider,
                         streamSource: StreamSource,
                         maxOrder: Int,
                         pMin: Double,
                         alpha: Double,
                         gammaMin: Double,
                         r: Double
                       ): List[(PredictionSuffixTree, SymbolMapper)] = {
    val dsrais = dsrap.provide()
    val pstWithSMs = dsrais.map(dsrai => learnPST(dsrai, streamSource, maxOrder, pMin, alpha, gammaMin, r))
    pstWithSMs
  }

  /**
    * Transforms PSTs from the given CSTs and ISOs.
    *
    * @param cstIsosOrder The list of CST, Iso and maximum order. One triplet for each pst.
    * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded (same for all PSTs).
    * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold (same for all PSTs).
    * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold (same for all PSTs).
    * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the
    *                     conditional on the expanded context by the conditional on the original context is greater than
    *                     this threshold (same for all PSTs).
    * @return             A list of prediction suffix trees and their symbol mappers.
    */
  private def transformToPSTs(
                         cstIsosOrder: List[(CounterSuffixTree, Isomorphism, Int)],
                         pMin: Double,
                         alpha: Double,
                         gammaMin: Double,
                         r: Double
                       ): List[(PredictionSuffixTree, SymbolMapper)] = {
    val pstWithSMs = cstIsosOrder.map(cstIsoOrder => VMMUtils.learnPredictionSuffixTree(cstIsoOrder._2, cstIsoOrder._1, cstIsoOrder._3, pMin, alpha, gammaMin, r, withMissing = false))
    pstWithSMs
  }

  /**
    * Learns a prediction suffix tree from a SDFA and a training stream.
    *
    * @param sdfai        The SDFA interface,
    * @param streamSource The source for the training stream.
    * @param maxOrder     The maximum order of the PST.
    * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold.
    * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                     expanded context must be greater than this threshold.
    * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the
    *                     conditional on the expanded context by the conditional on the original context is greater than
    *                     this threshold.
    * @return             A prediction suffix tree and its symbol mapper.
    */
  private def learnPST(
                        sdfai: SDFAInterface,
                        streamSource: StreamSource,
                        maxOrder: Int,
                        pMin: Double,
                        alpha: Double,
                        gammaMin: Double,
                        r: Double
                      ): (PredictionSuffixTree, SymbolMapper) = {
    val sdfa = sdfai.sdfa
    val partitionAttribute = sdfai.partitionAttribute
    val (pst, sm) = VMMUtils.learnPredictionSuffixTree(
      streamSource,
      sdfa,
      partitionAttribute,
      maxOrder,
      pMin,
      alpha,
      gammaMin,
      r
    )
    (pst, sm)
  }

  /**
   * Learns a prediction suffix tree from a dSRA and a training stream.
   *
   * @param dsrai        The dSRA interface,
   * @param streamSource The source for the training stream.
   * @param maxOrder     The maximum order of the PST.
   * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded.
   * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
   *                     context must be greater than this threshold.
   * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
   *                     expanded context must be greater than this threshold.
   * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
   *                     on the expanded context by the conditional on the original context is greater than this threshold.
   * @return             A prediction suffix tree and its symbol mapper.
   */
  private def learnPST(
                        dsrai: DSRAInterface,
                        streamSource: StreamSource,
                        maxOrder: Int,
                        pMin: Double,
                        alpha: Double,
                        gammaMin: Double,
                        r: Double
                      ): (PredictionSuffixTree, SymbolMapper) = {
    val dsra = dsrai.dsra
    val partitionAttribute = dsrai.partitionAttribute
    val (pst, sm) = VMMUtils.learnPredictionSuffixTree(
      streamSource,
      dsra,
      partitionAttribute,
      maxOrder,
      pMin,
      alpha,
      gammaMin,
      r
    )
    (pst, sm)
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
