package fsm.symbolic

import fsm.symbolic.logic.{EpsilonSentence, Sentence}
import stream.GenericEvent

abstract class Guard (val sentence: Sentence) extends Serializable {
  def check(event: GenericEvent): Boolean = sentence.evaluate(event)

  def isEpsilon: Boolean = sentence.isInstanceOf[EpsilonSentence]

  def isSentence(s: Sentence): Boolean = s == sentence

}
