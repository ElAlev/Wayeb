package workflow.provider.source.rt

import estimator.RemainingTimeEstimator.RemainingTimes

object RTSourceDirect {
  def apply(rts: List[RemainingTimes]): RTSourceDirect = new RTSourceDirect(rts)
}

class RTSourceDirect(val rts: List[RemainingTimes]) extends RTSource {

}
