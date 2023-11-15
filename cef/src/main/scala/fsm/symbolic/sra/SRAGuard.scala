package fsm.symbolic.sra

import fsm.symbolic.logic.Sentence
import fsm.symbolic.{Guard, Valuation}
import stream.GenericEvent

object SRAGuard {
  def apply(sentence: Sentence): SRAGuard = new SRAGuard(sentence)
}

class SRAGuard private[sra](override val sentence: Sentence) extends Guard(sentence = sentence) with Serializable {

  def check(
             event: GenericEvent,
             valuation: Valuation
           ): Boolean = sentence.evaluate(event, valuation)


}
