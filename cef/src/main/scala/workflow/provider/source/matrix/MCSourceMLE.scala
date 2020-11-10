package workflow.provider.source.matrix

import stream.source.StreamSource
import workflow.provider.FSMProvider

object MCSourceMLE {
  def apply(
             fsmp: FSMProvider,
             streamSource: StreamSource
           ): MCSourceMLE = new MCSourceMLE(fsmp, streamSource)
}

class MCSourceMLE(
                   val fsmp: FSMProvider,
                   val streamSource: StreamSource
                 ) extends MatrixSource {

}
