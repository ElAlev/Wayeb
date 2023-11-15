package fsm.symbolic.sfa.snfa

import fsm.symbolic.Constants
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sfa.{SFA, SFATransition}
import stream.GenericEvent

import java.io.{FileOutputStream, ObjectOutputStream}

/**
  * Class representing symbolic non-deterministic finite automata (SNFA).
  *
  * @param states The states of the SNFA, as a map of state ids and SNFAStates.
  * @param transitions The list of transitions.
  * @param start The id of the start state.
  * @param finals The set of ids of the final states.
  */
case class SNFA private[snfa] (
                                states: Map[Int, SNFAState],
                                transitions: List[SFATransition],
                                override val start: Int,
                                override val finals: Set[Int]
                              ) extends SFA(states, transitions, start, finals) with Serializable {

  private var dead: Set[Int] = Set.empty[Int]
  // The structure of the automaton is encoded in the list of transitions. Each transition has a source and target
  // state. This encoding can be inefficient for certain operations, e.g., for finding all successor states of a certain
  // state. In order to make these operations faster, we also encode the SNFA as a linked data structure, i.e., as a
  // graph where each state has direct references to its next and previous states.
  val graph: SNFAMutantGraph = SNFAMutantGraph(this)

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#getSuccessors(int, fsm.symbolic.sfa.logic.Sentence, scala.collection.immutable.Set).
    * The difference is that we use the graph to find the successors. More efficient.
    *
    * @param stateId The id of the state for which we want to find its successors.
    * @param sentence The sentence that is assumed to be true.
    * @param exclusives The set of exclusives.
    * @return All the successors that can be reached if the sentence is true.
    */
  def getSuccessorsFromGraph(
                              stateId: Int,
                              sentence: Sentence,
                              exclusives: Set[Set[Predicate]]
                            ): Set[Int] = {
    graph.getSuccessors(stateId, sentence, exclusives)
  }

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#getSuccessorsFromGraph(int, fsm.symbolic.sfa.logic.Sentence, scala.collection.immutable.Set).
    * The difference is that we start from multiple states.
    *
    * @param statesIds The ids of the states for which we want to find successors.
    * @param sentence The sentence that is assumed to be true.
    * @param exclusives The set of exclusives.
    * @return All the successors that can be reached if the sentence is true.
    */
  def getSuccessorsFromGraph(
                              statesIds: Set[Int],
                              sentence: Sentence,
                              exclusives: Set[Set[Predicate]]
                            ): Set[Int] = {
    statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getSuccessorsFromGraph(x, sentence, exclusives) }
  }

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#getEpsilonDelta(int). The difference is that we use the graph. More efficient.
    *
    * @param stateId The id of the given state for which we want to find its epsilon targets.
    * @return The ids of the epsilon targets.
    */
  def getEpsilonDeltaFromGraph(stateId: Int): Set[Int] = graph.getEpsilonDelta(stateId)

  /**
    * Finds the states reached from the given state, with the given input event, taking into account epsilon
    * transitions. All epsilon transitions from the given state and from the states reached with the event must also be
    * included.
    *
    * @param stateId The id of the given state.
    * @param event The input event.
    * @return The states reached with the input event, also following any epsilon transitions.
    */
  override def getDeltaWithEpsilon(
                                    stateId: Int,
                                    event: GenericEvent
                                  ): Set[Int] = {
    require(states.contains(stateId))
    val statesEpsilon = enclose(stateId)
    val statesEpsEv = statesEpsilon.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getDelta(x, event).map(_.stateId) }
    val delta = enclose(statesEpsEv)
    delta
  }

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#enclose(int), but uses the graph. More efficient.
    *
    * @param stateId The id of the state whose enclosure we want to find.
    * @return The enclosure of the state.
    */
  def encloseFromGraph(stateId: Int): Set[Int] = encloseAuxFromGraph(List(stateId), Set.empty[Int], Set.empty[Int])

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#encloseFromGraph(int).
    * The difference is that we find the enclosure of multiple states.
    *
    * @param statesIds The ids of the states whose enclosure we want to find.
    * @return The enclosure of the states.
    */
  def encloseFromGraph(statesIds: Set[Int]): Set[Int] = {
    statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ encloseFromGraph(x) }
  }

  @scala.annotation.tailrec
  private def encloseAuxFromGraph(
                                   toCheck: List[Int],
                                   checked: Set[Int],
                                   enclosure: Set[Int]
                                 ): Set[Int] = {
    val unchecked = toCheck.filter(s => !checked.contains(s))
    unchecked match {
      case Nil => enclosure
      case head :: tail => {
        val deltaEpsilon = getEpsilonDeltaFromGraph(head)
        val newToCheck = (tail.toSet ++ deltaEpsilon).toList
        val newChecked = checked + head
        val newEnclosure = (enclosure ++ deltaEpsilon) + head
        encloseAuxFromGraph(newToCheck, newChecked, newEnclosure)
      }
    }
  }

  def hasDead: Boolean = dead.nonEmpty

  def getDead: Int = {
    require(dead.nonEmpty)
    dead.head
  }

  def getDeads: Set[Int] = dead

  def setStateAsDead(s: Int): Unit = {
    //require(s == -1 | states.contains(s))
    //dead = if (s == -1) Set.empty[Int] else Set(s)
    require(s == Constants.deadStateIdConstant | states.contains(s))
    dead = if (s == Constants.deadStateIdConstant) Set.empty[Int] else Set(s)
  }

  def setStatesAsDead(ds: Set[Int]): Unit = {
    require(ds.forall(s => states.contains(s)))
    dead = ds
  }

  def statesConnectedToDead: Set[Int] = {
    val transitions2Dead = transitions.filter(t => dead.contains(t.target))
    val states2Dead = transitions2Dead.map(t => t.source).toSet &~ dead
    states2Dead
  }

  def statesNotConnectedToDead: Set[Int] = {
    val no2dead = (states.keySet &~ statesConnectedToDead) &~ dead
    val noEpsilonOnly = no2dead.filter(s => !epsilonOutgoing(s))
    val noOutgoing = states.keySet &~ transitions.map(t => t.source).toSet
    noEpsilonOnly ++ noOutgoing
  }

  private def epsilonOutgoing(s: Int): Boolean = {
    transitions.filter(t1 => t1.source == s).forall(t2 => t2.isEpsilon)
  }

  def outgoingSentences(s: Int): List[Sentence] = {
    val tr = transitions.filter(t => t.source == s & !t.isEpsilon)
    tr.map(t => t.guard.sentence)
  }

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
