package workflow.provider.source.hmm

import estimator.HMMEstimator.IsoHMM

object HMMSourceDirect {
  def apply(hmms: List[IsoHMM]): HMMSourceDirect = new HMMSourceDirect(hmms)
}

class HMMSourceDirect(val hmms: List[IsoHMM]) extends HMMSource {

}
