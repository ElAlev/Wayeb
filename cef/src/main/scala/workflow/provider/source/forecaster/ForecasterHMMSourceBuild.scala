package workflow.provider.source.forecaster

import model.waitingTime.ForecastMethod.ForecastMethod
import workflow.provider.{FSMProvider, HMMProvider}

object ForecasterHMMSourceBuild {
  def apply(
             fsmp: FSMProvider,
             hmmp: HMMProvider,
             horizon: Int,
             confidenceThreshold: Double,
             maxSpread: Int,
             method: ForecastMethod
           ): ForecasterHMMSourceBuild =
    new ForecasterHMMSourceBuild(fsmp, hmmp, horizon, confidenceThreshold, maxSpread, method)
}

class ForecasterHMMSourceBuild(
                                val fsmp: FSMProvider,
                                val hmmp: HMMProvider,
                                val horizon: Int,
                                val confidenceThreshold: Double,
                                val maxSpread: Int,
                                val method: ForecastMethod
                             ) extends ForecasterSource {

}
