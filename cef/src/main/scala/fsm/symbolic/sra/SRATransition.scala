package fsm.symbolic.sra

import fsm.symbolic.TransitionOutput.{IGNORE, TAKE, TransitionOutput}
import fsm.symbolic.{Transition, Valuation}
import stream.GenericEvent
import utils.StringUtils.list2Str

object SRATransition {
  def apply(
             source: Int,
             target: Int,
             guard: SRAGuard,
             output: TransitionOutput,
             writeRegisters: Set[String]
           ): SRATransition = new SRATransition(source, target, guard, output, writeRegisters)

  def apply(
             source: Int,
             target: Int,
             guard: SRAGuard,
             output: TransitionOutput
           ): SRATransition = new SRATransition(source, target, guard, output, Set.empty)

  def apply(
             source: Int,
             target: Int,
             guard: SRAGuard,
             writeRegisters: Set[String]
           ): SRATransition =
    new SRATransition(source, target, guard, TAKE, writeRegisters)

  def apply(
             source: Int,
             target: Int,
             guard: SRAGuard
           ): SRATransition =
    if (guard.isEpsilon) {
      //System.out.println("Trans IGNOR" + guard.toString)
      new SRATransition(source, target, guard, IGNORE, Set.empty)
    }
    else {
      //System.out.println("Trans TAKE" + guard.toString)
      new SRATransition(source, target, guard, TAKE, Set.empty)
    }
}

/**
 * Class representing transitions of SRA.
 * Besides the source/target states and the guard, SRA transitions also have a set of write registers.
 * When such a transition is triggered, it needs to write the triggering event to all of its write registers.
 *
 * @param source The id of the source state.
 * @param target The id of the target state.
 * @param guard The guard that must evaluate to true for the transition to be triggered.
 * @param writeRegisters The set of write registers, given as a set of strings.
 */
class SRATransition(
                     source: Int,
                     target: Int,
                     guard: SRAGuard,
                     output: TransitionOutput,
                     val writeRegisters: Set[String]
                   ) extends Transition(source, target, guard, output) {

  /**
   * Evaluates the transition against an event and a valuation.
   *
   * @param event The event to check.
   * @param valuation The given valuation.
   * @return True if the transition is triggered.
   */
  def enabled(
               event: GenericEvent,
               valuation: Valuation
             ): Boolean = {
    guard.check(event, valuation)
  }

  /**
   * Determines whether the transition can move to its target state, according to a given event and a given valuation.
   * If yes, the new valuation is also returned. If no, an empty valuation is returned.
   *
   * @param event The given event.
   * @param valuation The given valuation.
   * @return (true, new valuation) if the target state can be reached, (false, empty valuation) otherwise.
   */
  def yields(
            event: GenericEvent,
            valuation: Valuation
            ): (Boolean, Valuation) = {
    if (enabled(event, valuation)) {
      val newValuation = valuation.update(writeRegisters, event)
      (true, newValuation)
    }
    else (false, Valuation())
  }

  def hasWriteRegister(register: String): Boolean = writeRegisters.contains(register)

  override def toString: String = source + "-->" + target + "\t(" + guard.sentence.toString + ")[" + list2Str(writeRegisters.toList) + "]\t" + output


}
