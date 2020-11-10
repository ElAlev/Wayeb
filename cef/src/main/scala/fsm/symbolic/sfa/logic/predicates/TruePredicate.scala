package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent

/**
  * Implementation of the true predicate, a predicate for transitions that are always triggered, for every event.
  */
case class TruePredicate() extends Predicate {
  override def evaluate(event: GenericEvent): Boolean = true
}
