package fsm.symbolic.sfa

import fsm.symbolic.sfa.logic.Sentence
import stream.GenericEvent

object Transition {
  /**
    * Constructor for transition.
    *
    * @param source The id of the source state.
    * @param target The id of the target state.
    * @param guard The transition guard, holding the transition sentence.
    * @return The transition.
    */
  def apply(
      source: Int,
      target: Int,
      guard: Guard
  ): Transition =
    new Transition(source, target, guard)
}

/**
  * Class representing transitions of symbolic automata.
  *
  * @param source The id of the source state.
  * @param target The id of the target state.
  * @param guard The transition guard, holding the transition sentence.
  */
class Transition(
                  val source: Int,
                  val target: Int,
                  val guard: Guard
                ) extends Serializable {

  /**
    * Evaluates  the transition against an event.
    *
    * @param event The event to check.
    * @return True if the transition is triggered.
    */
  def enabled(event: GenericEvent): Boolean = {
    guard.check(event)
  }

  /**
    * Checks whether the transition is equipped with a given sentence.
    *
    * @param withSentence The given sentence.
    * @return True if the transition is equipped with the sentence.
    */
  def equipped(withSentence: Sentence): Boolean = guard.isSentence(withSentence)

  /**
    * Checks whether this is an epsilon transition.
    *
    * @return True if epsilon.
    */
  def isEpsilon: Boolean = guard.isEpsilon

  override def toString: String = {
    source + "-->" + target + "\t(" + guard.sentence.toString + ")"
  }

  def toStringSentence: String = guard.sentence.toString
}
