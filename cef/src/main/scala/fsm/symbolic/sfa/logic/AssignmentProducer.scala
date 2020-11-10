package fsm.symbolic.sfa.logic

/**
  * Creates all valid assignments of predicates according to a set of exclusives.
  *
  * NOTE: Used to be a singleton so that we don't have to go through this process every time, even when the exclusives
  * are the same. However, this can lead to problems when running tests in parallel. Singleton shared by all threads.
  *
  * @param exclusives The set of exclusives from which the valid assignments will be built.
  */
class AssignmentProducer(val exclusives: Set[Set[Predicate]]) {

  private val assignments: List[Assignment] = produceExclusiveAssignmentsSet(exclusives)

  /**
    * @return All created assignments.
    */
  def getAssignments: List[Assignment] = assignments

  /**
    * First gathers all predicates from all exclusives, creates all possible assignments and then filters out those
    * assignments that are not valid. If exclusives is an empty set, we also create an empty set.
    *
    * @param exclusives Sets of exclusives.
    * @return A list of valid assignments.
    */
  private def produceExclusiveAssignmentsSet(exclusives: Set[Set[Predicate]]): List[Assignment] = {
    if (exclusives.isEmpty) List.empty[Assignment]
    else {
      val allPredicates = exclusives.foldLeft(Set.empty[Predicate]) { (acc, x) => acc ++ x }.toList
      val boolPerm = new BooleanPermutator(allPredicates.size)
      val truthPerms = boolPerm.getPermutations(allPredicates.size)
      //val truthPerms = SingletonBooleanPermutator.getPermutations(allPredicates.size)
      val allAssignements = truthPerms.map(tp => Assignment(allPredicates.zip(tp)))
      val validAssignements = allAssignements.filter(ass => ass.isValidSet(exclusives))
      validAssignements.toList
    }
  }

/*
  private def produceExclusiveAssignments(exclusive: Set[Predicate]): List[Assignment] = {
    val el = exclusive.toList
    val falses = Array.fill(el.size){ false }.toList
    val falsesAssign = Assignment(el.zip(falses))
    val others = falses.indices.toList.map(i => falses.updated(i, true))
    val othersAssign = others.map(l => Assignment(el.zip(l)))
    falsesAssign :: othersAssign
  }

  // Produces pairs of (exclusive) predicates and for each pair, both predicates set to true
  private def produceNonExclusiveAssignments(exclusive: Set[Predicate]): List[Assignment] = {
    val el = exclusive.toList
    val pairs = el.indices.toList.
      map(index => el.drop(index + 1).map(pred => (el(index), pred))).
      filter(subpairs => subpairs.nonEmpty).
      flatten
    val ass = pairs.map(pair => Assignment(List((pair._1, true), (pair._2, true))))
    ass
  }
 */
}
