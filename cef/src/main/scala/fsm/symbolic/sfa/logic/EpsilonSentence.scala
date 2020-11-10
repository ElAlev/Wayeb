package fsm.symbolic.sfa.logic

import fsm.symbolic.sfa.logic.predicates.EpsilonPredicate

/**
  * A special class representing sentences for epsilon transitions.
  *
  * @param predicate The predicate which must be an epsilon predicate.
  */
class EpsilonSentence(predicate: Predicate) extends AtomicSentence(predicate) {
  require(predicate.isInstanceOf[EpsilonPredicate])
}
