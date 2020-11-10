package fsm.symbolic.sfa.logic

/**
  * A class that holds the truth values for a list of predicates, encoded as a map from predicates to their truth
  * values.
  *
  * @param assign a list of Tuple2 with predicates and their truth values.
  */
case class Assignment(assign: List[(Predicate, Boolean)]) {
  private val assMap: Map[Predicate, Boolean] = assign.toMap
  def getValueOf(p: Predicate): Boolean = {
    require(assMap.contains(p))
    assMap(p)
  }

  def contains(p: Predicate): Boolean = assMap.contains(p)

  /**
    * Checks if a set of (possibly) mutually exclusive predicates is actually valid, i.e., at most one can be true given
    * this assignment.
    *
    * @param exclusive A set of predicates to be checked whether they are mutually exclusive.
    * @return True if the predicates are indeed exclusive for this assignment.
    */
  def isValid(exclusive: Set[Predicate]): Boolean = {
    val filtered = assign.filter(a => exclusive.contains(a._1))
    val exclusiveAndTrue = filtered.filter(a => a._2)
    (exclusiveAndTrue.size <= 1) // at most one of the exclusive predicates can be true
  }

  /**
    * Same as fsm.symbolic.sfa.logic.Assignment#isValid(scala.collection.immutable.Set) above, but checks multiple sets
    * of exclusives.
    * @param exclusives A set of possible exclusives to be checked.
    * @return True if all sets of possible exclusives are indeed exclusive.
    */
  def isValidSet(exclusives: Set[Set[Predicate]]): Boolean = {
    exclusives.forall(exclusive => isValid(exclusive))
  }

  override def toString: String = {
    val str = utils.StringUtils.list2Str(assign, "\t")
    str
  }
}

