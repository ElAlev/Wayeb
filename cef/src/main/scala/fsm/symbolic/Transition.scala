package fsm.symbolic

import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.logic.Sentence
import fsm.symbolic.sra.SRATransition
import stream.GenericEvent


/**
 * Abstract class representing automaton transitions.
 *
 * @param source The id of the source state.
 * @param target The id of the target state.
 * @param guard The guard that must evaluate to true for the transition to be triggered.
 * @param output The output emitted by the transition.
 */
abstract class Transition(
                           val source: Int,
                           val target: Int,
                           val guard: Guard,
                           val output: TransitionOutput
                         ) extends Serializable {
  /**
   * Evaluates  the transition against an event.
   *
   * @param event The event to check.
   * @return True if the transition is triggered.
   */
  def enabled(event: GenericEvent): Boolean = guard.check(event)

  /**
   * Checks whether this is an epsilon transition.
   *
   * @return True if epsilon.
   */
  def isEpsilon: Boolean = guard.isEpsilon

  /**
   * Checks whether the transition is equipped with a given sentence.
   *
   * @param withSentence The given sentence.
   * @return True if the transition is equipped with the sentence.
   */
  def equipped(withSentence: Sentence): Boolean = guard.isSentence(withSentence)

  override def toString: String = source + "-->" + target + "\t(" + guard.sentence.toString + ")\t" + output


  def toStringSentence: String = guard.sentence.toString

}
