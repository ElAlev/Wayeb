package fsm.symbolic.sfa.logic

import fsm.symbolic.sfa.logic.predicates.IsEventTypePredicate

/**
  * A special class representing sentences for transitions that simply check the event type.
  *
  * @param predicate The predicate which must be an event type predicate.
  */
class IsEventTypeSentence(predicate: Predicate) extends AtomicSentence(predicate) {
  require(predicate.isInstanceOf[IsEventTypePredicate])
}
