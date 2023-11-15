package fsm.symbolic

import Constants.deadStateIdConstant
import fsm.symbolic.sfa.snfa.SNFA
import fsm.symbolic.sra.nsra.NSRA

object StateMapper {
  def apply(qdStart: Set[Int]): StateMapper = new StateMapper(qdStart)

  def apply(fa: Automaton): StateMapper = {
    fa match {
      case x: SNFA => {
        val qdStart = x.encloseFromGraph(x.start)
        new StateMapper(qdStart)
      }
      case x: NSRA => {
        val qdStart = x.enclose(x.start)
        new StateMapper(qdStart)
      }
      case _ => throw new IllegalArgumentException("apply method for StateMapper only accepts SNFA/NSRA automaton types")
    }

  }
}

/**
 * Keeps track of the states of a deterministic automaton as it is built and their correspondence to sets of states of
 * the non-deterministic automaton.
 *
 * @param qdStart The start state of the deterministic automaton as a set of states of the non-deterministic.
 */
class StateMapper private (qdStart: Set[Int]) {
  private val startId = deadStateIdConstant + 1
  // a mapping of SDFA states as unique ids to SDFA states as sets
  private val id2set = collection.mutable.Map[Int, Set[Int]](deadStateIdConstant -> Set.empty[Int], startId -> qdStart)
  // a mapping of SDFA states as sets to SDFA states as unique ids
  private val set2id = collection.mutable.Map[Set[Int], Int](Set.empty[Int] -> deadStateIdConstant, qdStart -> startId)
  private var maxId = startId

  /**
   * Adds a new deterministic state id.
   *
   * @param state The new state as a set of non-deterministic states.
   * @return The id of the new deterministic state.
   */
  def addNewDetState(state: Set[Int]): Int = {
    if (set2id.contains(state)) set2id(state)
    else {
      val newStateId = maxId + 1
      id2set.update(newStateId, state)
      set2id.update(state, newStateId)
      maxId = newStateId
      newStateId
    }
  }

  /**
   * Returns the set of non-deterministic states corresponding to the id of the deterministic state.
   *
   * @param state The id of the deterministic state.
   * @return The corresponding set of non-deterministic states.
   */
  def getAsSet(state: Int): Set[Int] = id2set(state)

  /**
   * Returns the id of the deterministic state corresponding to the set of non-deterministic states.
   *
   * @param set The set of non-deterministic states.
   * @return The corresponding id of the deterministic state.
   */
  def getAsId(set: Set[Int]): Int = set2id(set)

  def getStartId: Int = startId

}
