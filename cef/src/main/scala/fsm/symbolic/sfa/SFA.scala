package fsm.symbolic.sfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.logic.Sentence
import fsm.symbolic.sfa.sdfa.SDFA
import stream.GenericEvent

/**
  * Abstract class representing symbolic automata.
  *
  * @param states The states of the automaton as a map of IDs to states.
  * @param transitions The list of transitions.
  * @param start The id of the start state.
  * @param finals The set of IDs of the final states.
  */
abstract class SFA(
                    states: Map[Int, SFAState],
                    transitions: List[Transition],
                    start: Int,
                    finals: Set[Int]
                  ) extends Serializable with LazyLogging {
  require(start >= 0)
  require(states.keySet.contains(start))
  require(finals.forall(f => states.keySet.contains(f)))
  // Create a map of transitions keyed on the source state. Useful for faster access to transitions when searching based
  // on the source state.
  val transitionsMap: Map[Int, List[Transition]] = states.map(s => (
    s._1,
    //transitions.filter(t => t.source==s._1)
    //MiscUtils.shuffleList(transitions.filter(t => t.source==s._1))

    // This is to ensure that min-terms are evaluated in the same order for each state.
    // Not needed. Just to ensure consistency for throughput figures.
    sortTransitionsBySentence(transitions.filter(t => t.source == s._1))
  ))

  // Also create the set of all sentences. Do this upon creating because we might need to access the sentences multiple
  // times and need to avoid creating this set from scratch every time.
  val sentences: Set[Sentence] = transitions.map(t => t.guard.sentence).toSet

  /**
    * Gets the number of states.
    *
    * @return The number of states.
    */
  def size: Int = states.size

  /**
    * Every subclass should implement a method that determines whether a "word" of events is accepted by the SFA.
    *
    * @param events The "word" of events.
    * @return True if the :word" is accepted.
    */
  def accepts(events: List[GenericEvent]): Boolean

  /**
    * Finds the state(s) we can reach from a given state with a given event.
    *
    * @param fromStateId The id of the given state.
    * @param withEvent The given event.
    * @return The state(s) reached.
    */
  def getDelta(
                fromStateId: Int,
                withEvent: GenericEvent
              ): Set[Int] = {
    require(states.contains(fromStateId), "SFA\n" + this.toString + "\n does not have state: " + fromStateId)
    //val relevantTransitions = transitions.filter(t => t.source==s)
    val relevantTransitions = transitionsMap(fromStateId)
    val enabledTransitions = relevantTransitions.filter(t => !t.isEpsilon & t.enabled(withEvent))
    val targetStates = enabledTransitions.map(t => t.target)
    val result = targetStates.toSet // remove duplicates
    if (this.isInstanceOf[SDFA] & result.size != 1) {
      logger.error("Delta for SDFA should always return single state.")
      throw new Error("Delta for SDFA should always return single state.")
    }
    result
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
