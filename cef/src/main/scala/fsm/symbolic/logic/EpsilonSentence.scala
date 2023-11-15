package fsm.symbolic.logic

import fsm.symbolic.logic.predicates.EpsilonPredicate

/**
  * A special class representing sentences for epsilon transitions.
  *
  * @param predicate The predicate which must be an epsilon predicate.
  */
class EpsilonSentence(predicate: Predicate) extends AtomicSentence(predicate, Set.empty) {
  require(predicate.isInstanceOf[EpsilonPredicate])
}
