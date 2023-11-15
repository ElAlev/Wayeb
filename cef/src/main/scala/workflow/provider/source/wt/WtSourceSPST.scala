package workflow.provider.source.wt

import workflow.provider.SPSTProvider

object WtSourceSPST {
  def apply(
             spstProvider: SPSTProvider,
             horizon: Int,
             cutoffThreshold: Double,
             distance: (Double, Double)
           ): WtSourceSPST = new WtSourceSPST(spstProvider, horizon, cutoffThreshold, distance)

  def apply(
             spstProvider: SPSTProvider,
             horizon: Int,
             cutoffThreshold: Double
           ): WtSourceSPST = new WtSourceSPST(spstProvider, horizon, cutoffThreshold, distance = (0.0, 1.0))
}

class WtSourceSPST(
                    val spstProvider: SPSTProvider,
                    val horizon: Int,
                    val cutoffThreshold: Double,
                    val distance: (Double, Double)
                  ) extends WtSource {

}
