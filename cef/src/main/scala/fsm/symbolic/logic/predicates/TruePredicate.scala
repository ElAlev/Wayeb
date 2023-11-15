package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent

/**
  * Implementation of the true predicate, a predicate for transitions that are always triggered, for every event.
  */
case class TruePredicate(override val arguments: List[String]) extends Predicate(arguments) {
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean =  true
}
