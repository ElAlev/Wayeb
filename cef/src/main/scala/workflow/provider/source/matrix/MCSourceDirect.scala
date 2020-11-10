package workflow.provider.source.matrix

import model.markov.MarkovChain

object MCSourceDirect {
  def apply(mcs: List[MarkovChain]): MCSourceDirect = new MCSourceDirect(mcs)
}

class MCSourceDirect(val mcs: List[MarkovChain]) extends MatrixSource {

}
