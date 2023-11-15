package fsm.symbolic.sfa

import fsm.symbolic.Transition
import fsm.symbolic.TransitionOutput.{IGNORE, TAKE, TransitionOutput}

object SFATransition {
  def apply(
             source: Int,
             target: Int,
             guard: SFAGuard,
             output: TransitionOutput
           ): SFATransition = new SFATransition(source, target, guard, output)

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
      guard: SFAGuard
  ): SFATransition = {
    if (guard.isEpsilon) new SFATransition(source, target, guard, IGNORE)
    else new SFATransition(source, target, guard, TAKE)
  }
}

/**
  * Class representing transitions of symbolic automata.
  *
  * @param source The id of the source state.
  * @param target The id of the target state.
  * @param guard The transition guard, holding the transition sentence.
  * @param output The output emitted by the transition.
  */
class SFATransition(
                     source: Int,
                     target: Int,
                     guard: SFAGuard,
                     output: TransitionOutput
                   ) extends Transition(source, target, guard, output) with Serializable {
}
