package workflow.provider.source.wt

import workflow.provider.{FSMProvider, RemainingTimesProvider}

object WtSourceRT {
  def apply(
             fsmp: FSMProvider,
             rtps: RemainingTimesProvider,
             horizon: Int,
             finalsEnabled: Boolean
           ): WtSourceRT = new WtSourceRT(fsmp, rtps, horizon, finalsEnabled)
}

class WtSourceRT(
                  val fsmp: FSMProvider,
                  val rtps: RemainingTimesProvider,
                  val horizon: Int,
                  val finalsEnabled: Boolean
                ) extends WtSource {
  require(horizon > 0)

}
