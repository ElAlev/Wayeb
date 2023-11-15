package workflow.provider.source.hmm

import stream.source.StreamSource
import workflow.provider.FSMProvider

object HMMSourceEstimator {
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): HMMSourceEstimator = new HMMSourceEstimator(fsmp, streamSource)
}

class HMMSourceEstimator(
                          val fsmp: FSMProvider,
                          val streamSource: StreamSource
                        ) extends HMMSource {

}
