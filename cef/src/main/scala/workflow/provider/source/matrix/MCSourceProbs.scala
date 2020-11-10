package workflow.provider.source.matrix

import model.markov.TransitionProbs
import workflow.provider.FSMProvider

object MCSourceProbs {
  def apply(
             fsmp: FSMProvider,
             probs: TransitionProbs
           ): MCSourceProbs = new MCSourceProbs(
    fsmp,
    probs
  )
}
class MCSourceProbs(
                     val fsmp: FSMProvider,
                     val probs: TransitionProbs
                   ) extends MatrixSource {

}
