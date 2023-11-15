package workflow.provider.source.forecaster

import model.waitingTime.ForecastMethod.ForecastMethod
import ui.ConfigUtils
import workflow.provider.{FSMProvider, WtProvider}

object ForecasterSourceBuild {

  def apply(
             fsmp: FSMProvider,
             wtdp: WtProvider,
             horizon: Int,
             confidenceThreshold: Double,
             maxSpread: Int,
             method: ForecastMethod
           ): ForecasterSourceBuild = new ForecasterSourceBuild(
    fsmp,
    wtdp,
    horizon,
    confidenceThreshold,
    maxSpread,
    method
  )

  def apply(
             fsmp: FSMProvider,
             wtdp: WtProvider,
             horizon: Int,
             confidenceThreshold: Double,
             maxSpread: Int
           ): ForecasterSourceBuild = new ForecasterSourceBuild(
    fsmp,
    wtdp,
    horizon,
    confidenceThreshold,
    maxSpread,
    ConfigUtils.defaultForeMethod
  )

}

class ForecasterSourceBuild(
                             val fsmp: FSMProvider,
                             val wtdp: WtProvider,
                             val horizon: Int,
                             val confidenceThreshold: Double,
                             val maxSpread: Int,
                             val method: ForecastMethod
                          ) extends ForecasterSource {

}
