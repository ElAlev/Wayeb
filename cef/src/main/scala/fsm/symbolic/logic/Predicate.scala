package fsm.symbolic.logic

import fsm.symbolic.Valuation
import stream.GenericEvent

/**
  * Abstract class for representing predicates. All custom predicates must extend this class and reside under
  * fsm.symbolic.sfa.logic.predicates.
  */
abstract class Predicate(val arguments: List[String]) extends Serializable {
  /**
    * Each predicate must implement this method that evaluates it against an event and a valuation.
    *
    * @param event The event against which to evaluate the predicate.
    * @param valuation The valuation to be used, i.e., the register contents.
    * @return True if the predicate evaluates to true with the given event.
    */
  def evaluate(
                event: GenericEvent,
                valuation: Valuation
              ): Boolean

  def evaluate(event: GenericEvent): Boolean = evaluate(event, Valuation())

}
