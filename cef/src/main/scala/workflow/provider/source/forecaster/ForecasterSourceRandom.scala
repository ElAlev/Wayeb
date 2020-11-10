package workflow.provider.source.forecaster

import workflow.provider.FSMProvider

object ForecasterSourceRandom {
  def apply(
             fsmp: FSMProvider,
             horizon: Int
           ): ForecasterSourceRandom = new ForecasterSourceRandom(fsmp, horizon)
}

class ForecasterSourceRandom(
                             val fsmp: FSMProvider,
                             val horizon: Int
                           ) extends ForecasterSource {

}
