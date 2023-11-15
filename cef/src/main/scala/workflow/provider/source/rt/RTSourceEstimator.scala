package workflow.provider.source.rt

import stream.source.StreamSource
import workflow.provider.FSMProvider

object RTSourceEstimator {
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): RTSourceEstimator = new RTSourceEstimator(fsmp, streamSource)
}

class RTSourceEstimator(
                         val fsmp: FSMProvider,
                         val streamSource: StreamSource
                       ) extends RTSource {

}
