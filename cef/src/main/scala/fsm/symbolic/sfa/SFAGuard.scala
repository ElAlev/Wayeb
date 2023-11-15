package fsm.symbolic.sfa

import fsm.symbolic.Guard
import fsm.symbolic.logic.Sentence

object SFAGuard {
  def apply(sentence: Sentence): SFAGuard = new SFAGuard(sentence)
}

class SFAGuard private[sfa](sentence: Sentence) extends Guard(sentence = sentence) with Serializable {

}
