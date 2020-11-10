package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent

/**
  * Implementation of the epsilon predicate, a predicate for transitions that are always triggered, even without any
  * events. This is actually a pseudo-predicate. Real predicates are always evaluated against an event and must override
  * the evaluate function. An epsilon predicate does not really need to evaluate anything. This implementation is here
  * just for consistency purposes since every transition is required to have a guard with a sentence.
  * TODO: It would possibly make more sense to just create a subclass of fsm.symbolic.sfa.Transition for epsilon transitions that would not have a sentence.
  *
  * @param arguments does not matter
  */
case class EpsilonPredicate(arguments: List[Any]) extends Predicate {
  override def evaluate(event: GenericEvent): Boolean = true
}
