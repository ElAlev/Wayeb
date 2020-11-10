package workflow.provider.source.wt

import model.waitingTime.WtDistribution

object WtSourceDirect {
  def apply(wtds: List[Map[Int, WtDistribution]]): WtSourceDirect = new WtSourceDirect(wtds)
}

class WtSourceDirect(val wtds: List[Map[Int, WtDistribution]]) extends WtSource {

}
