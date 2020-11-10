package fsm.symbolic.sfa.snfa

import fsm.symbolic.sfa.logic.{Predicate, Sentence}
import fsm.symbolic.sfa.{SFA, Transition}
import stream.GenericEvent

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
                                transitions: List[Transition],
                                start: Int,
                                finals: Set[Int]
                              ) extends SFA(states, transitions, start, finals) {

  private var dead: Set[Int] = Set.empty[Int]
  // The structure of the automaton is encoded in the list of transitions. Each transition has a source and target
  // state. This encoding can be inefficient for certain operations, e.g., for finding all successor states of a certain
  // state. In order to make these operations faster, we also encode the SNFA as a linked data structure, i.e., as a
  // graph where each state has direct references to its next and previous states.
  val graph: SNFAMutantGraph = SNFAMutantGraph(this)

  /**
    * Returns true if the SNFA accepts the "string" of the given input events.
    *
    * @param events The input events.
    * @return True if the SNFA accepts the events.
    */
  override def accepts(events: List[GenericEvent]): Boolean = {
    require(events.nonEmpty)
    val reached = acceptsAux(events, Set(start))
    reached.intersect(finals).nonEmpty
  }

  @scala.annotation.tailrec
  private def acceptsAux(
                          events: List[GenericEvent],
                          reached: Set[Int]
                        ): Set[Int] = {
    events match {
      case Nil => reached
      case head :: tail => {
        val newReached = reached.foldLeft(Set.empty[Int]){ (acc, x) => acc ++ getDeltaWithEpsilon(x, head) }
        acceptsAux(tail, newReached)
      }
    }
  }

  /**
    * Returns the ids of the states that we can reach with the given event from all the given states.
    *
    * @param statesIds The ids of the given states.
    * @param event The input event.
    * @return The ids of the states that we can reach with the given event from all the given states
    */
  def getDelta(
                statesIds: Set[Int],
                event: GenericEvent
              ): Set[Int] = {
    statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getDelta(x, event) }
  }

  /**
    * Finds all the next states (successors) of a certain state, given a sentence and a set of exclusives, i.e.,
    * a next state is included only if the guard of the transition to it is triggered whenever the given sentence
    * is true (taking the exclusives into account).
    * CAUTION: Not efficient, use version with graph if performance is an issue.
    *
    * @param stateId The id of the state for which we want to find its successors.
    * @param sentence The sentence that is assumed to be true.
    * @param exclusives The set of exclusives.
    * @return All the successors that can be reached if the sentence is true.
    */
  def getSuccessors(
                     stateId: Int,
                     sentence: Sentence,
                     exclusives: Set[Set[Predicate]]
                   ): Set[Int] = {
    require(states.contains(stateId))
    val relevantTransitions = transitions.filter(t => t.source == stateId)
    val predTransitions = relevantTransitions.filter(t => !t.isEpsilon & sentence.entails(t.guard.sentence, exclusives))
    val targetStates = predTransitions.map(t => t.target)
    targetStates.toSet
  }

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#getSuccessors(int, fsm.symbolic.sfa.logic.Sentence, scala.collection.immutable.Set).
    * The difference is that we start from multiple states.
    *
    * @param statesIds The ids of the states for which we want to find successors.
    * @param sentence The sentence that is assumed to be true.
    * @param exclusives The set of exclusives.
    * @return All the successors that can be reached if the sentence is true.
    */
  def getSuccessors(
                     statesIds: Set[Int],
                     sentence: Sentence,
                     exclusives: Set[Set[Predicate]]
                   ): Set[Int] = {
    statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getSuccessors(x, sentence, exclusives) }
  }

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
    * Finds all the next states that can be accessed from the given state by following only epsilon transitions.
    *
    * @param stateId The id of the given state for which we want to find its epsilon targets.
    * @return The ids of the epsilon targets.
    */
  def getEpsilonDelta(stateId: Int): Set[Int] = {
    require(states.contains(stateId))
    val relevantTransitions = transitions.filter(t => t.source == stateId)
    val enabledTransitions = relevantTransitions.filter(t => t.isEpsilon)
    val targetStates = enabledTransitions.map(t => t.target)
    targetStates.toSet //.toList // remove duplicates
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
  def getDeltaWithEpsilon(
                           stateId: Int,
                           event: GenericEvent
                         ): Set[Int] = {
    require(states.contains(stateId))
    val statesEpsilon = enclose(stateId)
    val statesEpsEv = statesEpsilon.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getDelta(x, event) }
    val delta = enclose(statesEpsEv)
    delta
  }

  /**
    * Finds the enclosure of a state. For a definition of the enclosure of a state, see
    * @book{DBLP:books/daglib/0016921,
    *       author    = {John E. Hopcroft and
    *       Rajeev Motwani and
    *       Jeffrey D. Ullman},
    *       title     = {Introduction to automata theory, languages, and computation, 3rd Edition},
    *       series    = {Pearson international edition},
    *       publisher = {Addison-Wesley},
    *       year      = {2007}
    *
    * @param stateId The id of the state whose enclosure we want to find.
    * @return The enclosure of the state.
    */
  def enclose(stateId: Int): Set[Int] = encloseAux(List(stateId), Set.empty[Int], Set.empty[Int])

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#enclose(int), but uses the graph. More efficient.
    *
    * @param stateId The id of the state whose enclosure we want to find.
    * @return The enclosure of the state.
    */
  def encloseFromGraph(stateId: Int): Set[Int] = encloseAuxFromGraph(List(stateId), Set.empty[Int], Set.empty[Int])

  /**
    * Same as fsm.symbolic.sfa.snfa.SNFA#enclose(int). The difference is that we find the enclosure of multiple states.
    *
    * @param statesIds The ids of the states whose enclosure we want to find.
    * @return The enclosure of the states.
    */
  def enclose(statesIds: Set[Int]): Set[Int] = statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ enclose(x) }

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
  private def encloseAux(
                          toCheck: List[Int],
                          checked: Set[Int],
                          enclosure: Set[Int]
                        ): Set[Int] = {
    val unchecked = toCheck.filter(s => !checked.contains(s))
    unchecked match {
      case Nil => enclosure
      case head :: tail => {
        val deltaEpsilon = getEpsilonDelta(head)
        val newToCheck = (tail.toSet ++ deltaEpsilon).toList
        val newChecked = checked + head
        val newEnclosure = (enclosure ++ deltaEpsilon) + head
        encloseAux(newToCheck, newChecked, newEnclosure)
      }
    }
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

  def hasEpsilon: Boolean = transitions.exists(t => t.isEpsilon)

  def hasDead: Boolean = dead.nonEmpty

  def getDead: Int = {
    require(dead.nonEmpty)
    dead.head
  }

  def getDeads: Set[Int] = dead

  def setStateAsDead(s: Int): Unit = {
    require(s == -1 | states.contains(s))
    dead = if (s == -1) Set.empty[Int] else Set(s)
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

}
