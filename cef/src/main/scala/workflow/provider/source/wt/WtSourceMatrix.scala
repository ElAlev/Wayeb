package workflow.provider.source.wt

import workflow.provider.{FSMProvider, MarkovChainProvider}

object WtSourceMatrix {
  def apply(
             fsmp: FSMProvider,
             mcps: MarkovChainProvider,
             horizon: Int,
             finalsEnabled: Boolean
           ): WtSourceMatrix = new WtSourceMatrix(fsmp, mcps, horizon, finalsEnabled)
}

class WtSourceMatrix(
                      val fsmp: FSMProvider,
                      val mcps: MarkovChainProvider,
                      val horizon: Int,
                      val finalsEnabled: Boolean
) extends WtSource {
  require(horizon > 0)
}
