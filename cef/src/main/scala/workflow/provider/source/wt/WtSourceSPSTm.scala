package workflow.provider.source.wt

import workflow.provider.SPSTmProvider

object WtSourceSPSTm {
  def apply(
             spstmProvider: SPSTmProvider,
             horizon: Int,
             cutoffThreshold: Double,
             distance: (Double, Double)
           ): WtSourceSPSTm = new WtSourceSPSTm(spstmProvider, horizon, cutoffThreshold, distance)

  def apply(
             spstmProvider: SPSTmProvider,
             horizon: Int,
             cutoffThreshold: Double
           ): WtSourceSPSTm = new WtSourceSPSTm(spstmProvider, horizon, cutoffThreshold, distance = (0.0, 1.0))
}

class WtSourceSPSTm (
                      val spstmProvider: SPSTmProvider,
                      val horizon: Int,
                      val cutoffThreshold: Double,
                      val distance: (Double, Double)
                    ) extends WtSource {

}
