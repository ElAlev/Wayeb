package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import workflow.condition.Condition
import workflow.provider.source.wt.{WtSource, WtSourceDirect, WtSourceMatrix}
import model.waitingTime.WtDistribution

object WtProvider {
  /**
    * Constructor for waiting-time distributions provider.
    *
    * @param wtSource The waiting-time source:
    *                 - WtSourceDirect when the waiting-time distributions already exist;
    *                 - WtSourceMatrix when the distributions are to be built from a transition matrix;
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
      case _ => throw new Error("Not valid WtSource")
    }
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


}
