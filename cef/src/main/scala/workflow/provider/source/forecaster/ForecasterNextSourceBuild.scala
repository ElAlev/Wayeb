package workflow.provider.source.forecaster

import workflow.provider.{FSMProvider, MarkovChainProvider}

object ForecasterNextSourceBuild {
  def apply(
             fsmp: FSMProvider,
             mcp: MarkovChainProvider,
             confidenceThreshold: Double
           ): ForecasterNextSourceBuild = new ForecasterNextSourceBuild(fsmp, mcp, confidenceThreshold)
}

class ForecasterNextSourceBuild(
                                val fsmp: FSMProvider,
                                val mcp: MarkovChainProvider,
                                val confidenceThreshold: Double
                              ) extends ForecasterSource {

}
