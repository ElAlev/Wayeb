package fsm.symbolic.sfa.logic

import fsm.symbolic.sfa.logic.predicates.TruePredicate

/**
  * A special class representing sentences for true transitions, i.e., transitions that are triggered for every event.
  * True transitions are not the same as epsilon transitions. Epsilon transitions can be followed even without an event.
  * True transitions must consume an event.
  *
  * @param predicate The predicate which must be a True predicate.
  */
class TrueSentence(predicate: Predicate) extends AtomicSentence(predicate) {
  require(predicate.isInstanceOf[TruePredicate])
}
