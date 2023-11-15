package workflow.provider.source.psa

import stream.array.EventStream

object PSASourceLearner {
  def apply(trainStream: EventStream): PSASourceLearner = new PSASourceLearner(trainStream)
}

class PSASourceLearner(val trainStream: EventStream) extends PSASource {

}
