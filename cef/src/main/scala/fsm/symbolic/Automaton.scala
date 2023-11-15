package fsm.symbolic

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sra.Configuration
import stream.GenericEvent

/**
 * This is the abstract class from which all other automaton models inherit. Contains functionality common to all
 * automata.
 *
 * @param states The automaton states, as a map of state ids to states.
 * @param transitions The list of transitions.
 * @param start The id of the start state.
 * @param finals The set of ids of the final states.
 */
abstract class Automaton(
                          states: Map[Int, AutomatonState],
                          transitions: List[Transition],
                          val start: Int,
                          val finals: Set[Int]
                        ) extends Serializable with LazyLogging {
  require(start >= 0)
  require(states.keySet.contains(start))
  require(finals.forall(f => states.keySet.contains(f)))
  // Create a map of transitions keyed on the source state. Useful for faster access to transitions when searching based
  // on the source state.
  val transitionsMap: Map[Int, Array[Transition]] = states.map(s => (
    s._1,
    //transitions.filter(t => t.source==s._1)
    //MiscUtils.shuffleList(transitions.filter(t => t.source==s._1))

    // This is to ensure that min-terms are evaluated in the same order for each state.
    // Not needed. Just to ensure consistency for throughput figures.
    //sortTransitionsBySentence(transitions.filter(t => t.source == s._1))
    transitions.filter(t => t.source == s._1).toArray
  ))

  // Also create the set of all sentences. Do this upon creating because we might need to access the sentences multiple
  // times and need to avoid creating this set from scratch every time.
  val sentences: Set[Sentence] = transitions.map(t => t.guard.sentence).toSet

  val transitionsArray = transitions.toArray

  val transitionsMapMutable = collection.mutable.HashMap(transitionsMap.toSeq: _*)

  val transitionsMapAsArray: Array[Array[Transition]] = new Array[Array[Transition]](states.keySet.max + 1)

  states.keySet.foreach(state => {
    transitionsMapAsArray(state) = transitionsMap(state)
  })

  /**
   * Checks whether a given state is a final state.
   *
   * @param state The id of the given state.
   * @return True if it is a final state.
   */
  def isFinal(state: Int): Boolean = {
    require(states.contains(state))
    finals.contains(state)
  }

  /**
   * Checks whether a given state is the start state.
   *
   * @param state The id of the given state.
   * @return True if it is the start state.
   */
  def isStart(state: Int): Boolean = {
    require(states.contains(state))
    state == start
  }

  /**
   * Gets the number of states.
   *
   * @return The number of states.
   */
  def size: Int = states.size

  /**
   * Finds all the states we can reach from the given states with one transition.
   *
   * @param statesSet The ids of the given states.
   * @return The set of ids of the states we can reach.
   */
  def getNextStatesFromStates(statesSet: Set[Int]): Set[Int] = statesSet.flatMap(s => getNextStatesFromState(s))

  /**
   * Finds all the states we can reach from the given state with one transition.
   *
   * @param state The id of the given state.
   * @return The set of ids of the states we can reach.
   */
  def getNextStatesFromState(state: Int): Set[Int] = transitions.filter(t => t.source == state).map(t => t.target).toSet

  /**
   * Checks whether two states are connected with a transition.
   *
   * @param from The id of the candidate source state.
   * @param to The id of the candidate target state.
   * @return True if the two states are connected.
   */
  def connected(
                 from: Int,
                 to: Int
               ): Boolean = {
    val next = getNextStatesFromState(from)
    next.contains(to)
  }

  /**
   * Returns true if the automaton accepts the "string" of the given input events.
   *
   * @param events The input events.
   * @return True if the SNFA accepts the events.
   */
  def accepts(events: List[GenericEvent]): Boolean = {
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
   * Finds the states reached from the given state, with the given input event, taking into account epsilon
   * transitions. All epsilon transitions from the given state and from the states reached with the event must also be
   * included, if the automaton is non-deterministic.
   *
   * @param stateId The id of the given state.
   * @param event The input event.
   * @return The states reached with the input event, also following any epsilon transitions.
   */
  def getDeltaWithEpsilon(
                           stateId: Int,
                           event: GenericEvent
                         ): Set[Int]

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
    statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ getDelta(x, event).map(y => y.stateId) }
  }

  /**
   * Finds the configuration(s) we can reach from a given state with a given event.
   *
   * @param fromStateId The id of the given state.
   * @param withEvent The given event.
   * @return The configuration(s) reached.
   */
  def getDelta(
                fromStateId: Int,
                withEvent: GenericEvent
              ): Set[Configuration] = {
    require(states.contains(fromStateId), "Automaton\n" + this.toString + "\n does not have state: " + fromStateId)
    val relevantTransitions = transitionsMap(fromStateId)
    val enabledTransitions = relevantTransitions.filter(t => !t.isEpsilon & t.enabled(withEvent))
    val targetStates = enabledTransitions.map(t => Configuration(t.target, t.output))
    val result = targetStates//.toSet // remove duplicates
    result.toSet
  }

  def getDeltaNoConfArray(
                           fromStateId: Int,
                           withEvent: GenericEvent
                         ): List[(Int,TransitionOutput)] = {
    require(states.contains(fromStateId), "Automaton\n" + this.toString + "\n does not have state: " + fromStateId)
    val relevantTransitions = transitionsMap(fromStateId)
    val enabledTransitions = relevantTransitions.filter(t => !t.isEpsilon & t.enabled(withEvent))
    val targetStates = enabledTransitions.map(t => (t.target, t.output))
    val result = targetStates //.toSet // remove duplicates
    result.toList
  }

  def getDeltaNoConfArrayWhile(
                                fromStateId: Int,
                                withEvent: GenericEvent
                              ): List[(Int, TransitionOutput)] = {
    //require(states.contains(fromStateId), "Automaton\n" + this.toString + "\n does not have state: " + fromStateId)
    val relevantTransitions = transitionsMap(fromStateId)
    var i = 0
    var r = List.empty[(Int,TransitionOutput)]
    while (i < relevantTransitions.size) {
      val t = relevantTransitions(i)
      if (!t.isEpsilon & t.enabled(withEvent)) r = (t.target, t.output) :: r
      i += 1
    }
    r
  }

  def getDeltaNoConfArrayWhileMutableMap(
                                          fromStateId: Int,
                                          withEvent: GenericEvent
                                        ): List[(Int, TransitionOutput)] = {
    val relevantTransitions = transitionsMapMutable(fromStateId)
    var i = 0
    var r = List.empty[(Int, TransitionOutput)]
    while (i < relevantTransitions.size) {
      val t = relevantTransitions(i)
      if (!t.isEpsilon & t.enabled(withEvent)) r = (t.target, t.output) :: r
      i += 1
    }
    r
  }

  def getDeltaNoConfArrayWhileMutableMapPrealloc(
                                                  fromStateId: Int,
                                                  withEvent: GenericEvent
                                                ): Array[(Int, TransitionOutput)] = {
    val relevantTransitions = transitionsMapMutable(fromStateId)
    var i = 0
    val r: Array[(Int, TransitionOutput)] = new Array[(Int, TransitionOutput)](relevantTransitions.length)
    while (i < relevantTransitions.length) {
      val t = relevantTransitions(i)
      if (!t.isEpsilon & t.enabled(withEvent)) r(i) = (t.target, t.output)
      i += 1
    }
    r
  }

  def getDeltaNoConfArrayWhileTransitionsArrayPrealloc(
                                                        fromStateId: Int,
                                                        withEvent: GenericEvent
                                                      ): Array[(Int, TransitionOutput)] = {
    val relevantTransitions = transitionsMapAsArray(fromStateId)
    var i = 0
    val r: Array[(Int, TransitionOutput)] = new Array[(Int, TransitionOutput)](relevantTransitions.length)
    while (i < relevantTransitions.length) {
      val t = relevantTransitions(i)
      if (t.enabled(withEvent)) r(i) = (t.target, t.output)
      i += 1
    }
    r
  }

  def getDeltaNoConfArrayWhileNoMap(
                                     fromStateId: Int,
                                     withEvent: GenericEvent
                                   ): List[(Int, TransitionOutput)] = {
    var i = 0
    var r = List.empty[(Int, TransitionOutput)]
    while (i < transitionsArray.size) {
      val t = transitionsArray(i)
      if (t.source == fromStateId & !t.isEpsilon & t.enabled(withEvent)) r = (t.target, t.output) :: r
      i += 1
    }
    r
  }

  /**
   * Finds all the next states that can be accessed from the given state by following only epsilon transitions.
   *
   * @param stateId The id of the given state for which we want to find its epsilon targets.
   * @return The ids of the epsilon targets.
   */
  def getEpsilonOnlyDelta(stateId: Int): Set[Int] = {
    require(states.contains(stateId))
    val relevantTransitions = transitions.filter(t => t.source == stateId)
    val enabledTransitions = relevantTransitions.filter(t => t.isEpsilon)
    val targetStates = enabledTransitions.map(t => t.target)
    targetStates.toSet // remove duplicates
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
   * Same as fsm.symbolic.Automaton#enclose(scala.collection.immutable.Set).
   * The difference is that we find the enclosure of multiple states.
   *
   * @param statesIds The ids of the states whose enclosure we want to find.
   * @return The enclosure of the states.
   */
  def enclose(statesIds: Set[Int]): Set[Int] = statesIds.foldLeft(Set.empty[Int]) { (acc, x) => acc ++ enclose(x) }

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
        val deltaEpsilon = getEpsilonOnlyDelta(head)
        val newToCheck = (tail.toSet ++ deltaEpsilon).toList
        val newChecked = checked + head
        val newEnclosure = (enclosure ++ deltaEpsilon) + head
        encloseAux(newToCheck, newChecked, newEnclosure)
      }
    }
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
   * Same as fsm.symbolic.Automaton#getSuccessors(int, fsm.symbolic.logic.Sentence, scala.collection.immutable.Set).
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
   * Retrieves all sentences.
   *
   * @return All sentences of the SFA.
   */
  def getSentences: Set[Sentence] = sentences

  /**
   * Retrieves a set of stringified predicates of the SFA.
   *
   * @return All predicates of the SFA as strings.
   */
  def extractPredicateSymbols: Set[String] =
    transitions.foldLeft(Set.empty[String]) { (acc, t) => acc ++ t.guard.sentence.extractPredicateSymbols }

  /**
   * Retrieves the IDs of all states.
   *
   * @return the IDs of all states.
   */
  def getStates: Set[Int] = states.keySet

  /**
   * Retrieves all transitions sorted by source, target, sentence.
   *
   * @return All transitions sorted.
   */
  def getSortedTransitions: List[Transition] = transitions.sortWith((t1, t2) => transitionOrder(t1, t2))

  /**
   * Determines the order between two transitions. Order checks first source, then target, then sentence.
   *
   * @param t1 The first transition.
   * @param t2 The second transition.
   * @return True if the first transition if first in order.
   */
  private def transitionOrder(
                               t1: Transition,
                               t2: Transition
                             ): Boolean = {
    if (t1.source < t2.source) true //List(t1,t2)
    else if (t1.source == t2.source) {
      if (t1.target < t2.target) true //List(t1,t2)
      else if (t1.target == t2.target) {
        if (t1.guard.sentence.toString < t2.guard.sentence.toString) true //List(t1,t2)
        else false //List(t2,t1)
      } else false //List(t2,t1)
    } else false //List(t2,t1)
  }

  /**
   * Sorts transitions by sentence.
   *
   * @param transitions The transitions to be sorted.
   * @return The transitions sorted.
   */
  private def sortTransitionsBySentence(transitions: List[Transition]): List[Transition] = {
    val sortedTransitions = transitions.sortWith((t1, t2) => transitionOrderBySentence(t1, t2))
    sortedTransitions
  }

  /**
   * Determines the order between two transitions. Order checks only sentence.
   *
   * @param t1 The first transition.
   * @param t2 The second transition.
   * @return True if the first transition if first in order.
   */
  private def transitionOrderBySentence(
                                         t1: Transition,
                                         t2: Transition
                                       ): Boolean = {
    if (t1.guard.sentence.toString < t2.guard.sentence.toString) true
    else false
  }

  def hasEpsilon: Boolean = transitions.exists(t => t.isEpsilon)

  def hasOutgoing(state: Int): Boolean = transitions.exists(t => t.source == state)

  override def toString: String = {
    val sortedTransitions = getSortedTransitions
    val transitionsStr = utils.StringUtils.list2Str(sortedTransitions, "\n")
    val str = "States:\n" + states.keySet.toList.sorted.toString() + "\n" +
      "Start:\t" + start + "\n" +
      "Finals:\t" + finals + "\n" +
      "Transitions: \n" + transitionsStr
    str
  }

}
