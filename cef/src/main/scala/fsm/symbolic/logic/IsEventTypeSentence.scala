package fsm.symbolic.logic

import fsm.symbolic.logic.predicates.IsEventTypePredicate

/**
  * A special class representing sentences for transitions that simply check the event type.
  *
  * @param predicate The predicate which must be an event type predicate.
  */
class IsEventTypeSentence(predicate: Predicate) extends AtomicSentence(predicate, Set.empty) {
  require(predicate.isInstanceOf[IsEventTypePredicate])
}
