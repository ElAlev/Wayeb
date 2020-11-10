package fsm.symbolic.sfa

import fsm.symbolic.sfa.logic.{EpsilonSentence, Sentence}
import stream.GenericEvent

object Guard {
  def apply(sentence: Sentence): Guard = new Guard(sentence)
}

class Guard private[sfa] (val sentence: Sentence) extends Serializable {

  def check(event: GenericEvent): Boolean = sentence.evaluate(event)

  def isSentence(s: Sentence): Boolean = s == sentence

  def isEpsilon: Boolean = sentence.isInstanceOf[EpsilonSentence]

}
