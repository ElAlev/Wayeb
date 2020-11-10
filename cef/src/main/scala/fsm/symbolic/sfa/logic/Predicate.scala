package fsm.symbolic.sfa.logic

import stream.GenericEvent

/**
  * Abstract class for representing predicates. All custom predicates must extend this class and reside under
  * fsm.symbolic.sfa.logic.predicates.
  */
abstract class Predicate {
  /**
    * Each predicate must implement this method that evaluates it against an event.
    *
    * @param event The event against which to evaluate the predicate.
    * @return True if the predicate evaluates to true with the given event.
    */
  def evaluate(event: GenericEvent): Boolean
}
