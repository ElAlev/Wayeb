package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import workflow.condition.Condition
import workflow.provider.source.wt.{WtSource, WtSourceDirect, WtSourceMatrix, WtSourceRT, WtSourceSPST, WtSourceSPSTm}
import model.waitingTime.WtDistribution

object WtProvider {
  /**
    * Constructor for waiting-time distributions provider.
    *
    * @param wtSource The waiting-time source:
    *                 - WtSourceDirect when the waiting-time distributions already exist;
    *                 - WtSourceMatrix when the distributions are to be built from a transition matrix;
    *                 - WtSourceRT when the distributions are to be built from remaining times;
    *                 - WtSourceSPST when the distributions are to be built from SPST.
    * @return A waiting-time provider.
    */
  def apply(wtSource: WtSource): WtProvider = new WtProvider(wtSource, List.empty)
}

/**
  * According to type of waiting-time source, builds a provider in the form of a list of waiting-time distributions.
  *
  * @param wtSource The waiting-time source:
  *                 - WtSourceDirect when the waiting-time distributions already exist;
  *                 - WtSourceMatrix when the distributions are to be built from a transition matrix;
  *                 - WtSourceRT when the distributions are to be built from remaining times;
  *                 - WtSourceSPST when the distributions are to be built from SPST.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class WtProvider private (
                           wtSource: WtSource,
                           conditions: List[Condition]
                         ) extends AbstractProvider(conditions) with LazyLogging {

  /**
    * Calling this function actually initiates the construction of the distributions.
    *
    * @return A list of waiting-time distributions.
    */
  override def provide(): List[Map[Int, WtDistribution]] = {
    wtSource match {
      case x: WtSourceDirect => x.wtds
      case x: WtSourceMatrix => getWtdsFromMatrix(x.fsmp, x.mcps, x.horizon, x.finalsEnabled)
      case x: WtSourceRT => getWtdsFromRTs(x.fsmp, x.rtps, x.horizon, x.finalsEnabled)
      case x: WtSourceSPST => getWtdsFromSPST(x.spstProvider, x.horizon, x.cutoffThreshold, x.distance)
      case x: WtSourceSPSTm => getWtdsFromSPSTm(x.spstmProvider, x.horizon, x.cutoffThreshold, x.distance)
      case _ => throw new Error("Not valid WtSource")
    }
  }

  /**
    * Estimates waiting-time distributions from SPST.
    *
    * @param spstProvider The provider for the SPST.
    * @param horizon The horizon.
    * @param cutoffThreshold The cutoff threshold.
    * @param distance The distance.
    * @return A list of waiting-time distributions.
    */
  private def getWtdsFromSPST(
                               spstProvider: SPSTProvider,
                               horizon: Int,
                               cutoffThreshold: Double,
                               distance: (Double, Double)
                             ): List[Map[Int, WtDistribution]] = {
    logger.info("Computing waiting-time distributions from SPST ...")
    val spsts = spstProvider.provide()
    val wtds = spsts.map(spst => spst.computeWtDistsOpt(horizon, cutoffThreshold, distance))
    logger.info("done.")
    wtds
  }

  /**
   * Estimates waiting-time distributions from SPSTm.
   *
   * @param spstmProvider The provider for the SPSTm.
   * @param horizon The horizon.
   * @param cutoffThreshold The cutoff threshold.
   * @param distance The distance.
   * @return A list of waiting-time distributions.
   */
  private def getWtdsFromSPSTm(
                                spstmProvider: SPSTmProvider,
                                horizon: Int,
                                cutoffThreshold: Double,
                                distance: (Double, Double)
                             ): List[Map[Int, WtDistribution]] = {
    logger.info("Computing waiting-time distributions from SPSTm ...")
    val spstms = spstmProvider.provide()
    val wtds = spstms.map(spstm => spstm.computeWtDistsOpt(horizon, cutoffThreshold, distance))
    logger.info("done.")
    wtds
  }

  /**
    * Estimates waiting-time distributions from matrices.
    *
    * @param fsmProvider The provider for the FSMs.
    * @param matrixProvider The provider for the matrices.
    * @param horizon The horizon.
    * @param finalsEnabled If true, final states will emit forecasts.
    * @return A list of waiting-time distributions.
    */
  private def getWtdsFromMatrix(
                                 fsmProvider: FSMProvider,
                                 matrixProvider: MarkovChainProvider,
                                 horizon: Int,
                                 finalsEnabled: Boolean
                               ): List[Map[Int, WtDistribution]] = {
    logger.info("Computing waiting-time distributions from Markov chain ...")
    val fsms = fsmProvider.provide()
    val mcs = matrixProvider.provide()
    val wtds = fsms.zip(mcs).map(x => x._2.computeWTDists(x._1, horizon, finalsEnabled))
    logger.info("done.")
    wtds
  }

  /**
    * Estimates waiting-time distributions from remaining times.
    *
    * @param fsmProvider The provider for FSMs.
    * @param rtProvider The provider for remaining times.
    * @param horizon The horizon.
    * @param finalsEnabled If true, final states will emit forecasts.
    * @return A list of waiting-time distributions.
    */
  private def getWtdsFromRTs(
                              fsmProvider: FSMProvider,
                              rtProvider: RemainingTimesProvider,
                              horizon: Int,
                              finalsEnabled: Boolean
                            ): List[Map[Int, WtDistribution]] = {
    logger.info("Computing waiting-time distributions from remaining times ...")
    val fsms = fsmProvider.provide()
    val rts = rtProvider.provide()
    val wtds = fsms.zip(rts).map(x => x._2.computeWTDists(x._1, horizon, finalsEnabled))
    logger.info("done.")
    wtds
  }
}
