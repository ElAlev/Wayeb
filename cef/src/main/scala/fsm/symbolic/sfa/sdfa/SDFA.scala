package fsm.symbolic.sfa.sdfa

import java.io.{FileOutputStream, ObjectOutputStream}
import com.github.tototoshi.csv.CSVWriter
import fsm.symbolic.sfa.logic.Sentence
import fsm.symbolic.sfa.{SFA, Transition}
import stream.GenericEvent
import scala.util.control.Breaks._

object SDFA {
  def apply(
             states: Map[Int, SDFAState],
             transitions: List[Transition],
             start: Int,
             finals: Set[Int]
           ): SDFA = {
    val duplicates = states.keySet.map(s => (s -> Set.empty[Int])).toMap
    new SDFA(states, transitions, start, finals, duplicates)
  }

  def apply(
             states: Set[Int],
             transitions: List[Transition],
             start: Int,
             finals: Set[Int]
           ): SDFA = {
    val sdfaStates = states.map(s => (s, SDFAState(s))).toMap
    SDFA(sdfaStates, transitions, start, finals)
  }
}

/**
  * Class representing symbolic deterministic finite automata (SDFA).
  *
  * A SDFA may also have duplicate states, if it is a disambiguated SDFA. When we disambiguate a SDFA from k to k+1,
  * for each state of the k-SDFA we may create extra states. We call these extra states, the duplicates of the original
  * state.
  *
  * @param states The states of the SDFA, as a map of state ids and SDFAStates.
  * @param transitions The list of transitions.
  * @param start The id of the start state.
  * @param finals The set of ids of the final states.
  * @param duplicates A map, which, for each state, stores all its duplicates.
  */
case class SDFA private[sfa] (
                               states: Map[Int, SDFAState],
                               transitions: List[Transition],
                               start: Int,
                               finals: Set[Int],
                               duplicates: Map[Int, Set[Int]]
                             ) extends SFA(states, transitions, start, finals) with Serializable {

  private val deltaOnSentence: Map[(Int, Sentence), Set[Int]] = Map[(Int, Sentence), Set[Int]]() //buildDeltaOnSentence

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
    * Finds all the original states, i.e., states that are not duplicates.
    *
    * @return The set of ids of the original states.
    */
  private def findOriginals(): Set[Int] = {
    val si = states.keySet
    si.filter(s => !isDuplicate(s))
  }

  /**
    * Checks whether a given state is a duplicate of another state.
    *
    * @param state The id of the given state.
    * @return True if it is a duplicate.
    */
  private def isDuplicate(state: Int): Boolean = {
    duplicates.exists(sd => sd._2.contains(state))
  }

  /**
    * Returns a map where, for each original state, we find the set of all its deep duplicate states, i.e., its
    * duplicate states and the duplicates of the first duplicate states etc.
    *
    * @return The original states with their deep duplicates.
    */
  private def getDeepDuplicates(): Map[Int, Set[Int]] = {
    val originals = findOriginals()
    originals.map(o => (o, getDeepDuplicates(o))).toMap
  }

  /**
    * Finds all the deep duplicates of a given state.
    *
    * @param state The id of the given state.
    * @return All the deep duplicates of the given state.
    */
  private def getDeepDuplicates(state: Int): Set[Int] = {
    getDeepDuplicatesAux(List(state), Set.empty[Int])
  }

  /**
    * Recursive, auxiliary function to find all the deep duplicates of a given state.
    *
    * @param front The remaining states we need to check in every iteration. For the first call, the front should be
    *              just the given state.
    * @param dups Accumulator for all duplicate states. Empty for the first call.
    * @return
    */
  @scala.annotation.tailrec
  private def getDeepDuplicatesAux(
                                    front: List[Int],
                                    dups: Set[Int]
                                  ): Set[Int] = {
    front match {
      case Nil => dups
      case head :: tail => {
        val newDups = duplicates(head)
        val newFront = (tail.toSet ++ newDups).toList
        getDeepDuplicatesAux(newFront, dups ++ newDups)
      }
    }
  }

  /**
    * Basically the same as fsm.symbolic.sfa.sdfa.SDFA#getDeepDuplicates().
    *
    * @return A list of original states with their deep duplicates. The only difference is that each set of duplicates
    *         also contains the original state.
    */
  def traverse(): List[(Int, Set[Int])] = {
    val originals = findOriginals()
    val traversed = traverseAux(originals - start, Set(start), List.empty[Set[Int]])
    val m = traversed.map(t => (originals.intersect(t).head, t))
    m
  }

  @scala.annotation.tailrec
  private def traverseAux(
                           originalsLeft: Set[Int],
                           front: Set[Int],
                           traversed: List[Set[Int]]
                         ): List[Set[Int]] = {
    require(originalsLeft.intersect(front).isEmpty)
    if (front.isEmpty) traversed
    else {
      val newSets = front.toList.map(x => getDeepDuplicates(x) + x)
      val newTraversed = traversed ++ newSets
      val newFront = getNextStates(newSets.flatten.toSet).intersect(originalsLeft)
      val newOriginalsLeft = originalsLeft -- newFront
      traverseAux(newOriginalsLeft, newFront, newTraversed)
    }
  }

  /**
    * Finds all the states we can reach from the given states with one transition.
    *
    * @param statesSet The ids of the given states.
    * @return The set of ids of the states we can reach.
    */
  private def getNextStates(statesSet: Set[Int]): Set[Int] = {
    statesSet.flatMap(s => getNextStates(s))
  }

  /**
    * Finds all the states we can reach from the given state with one transition.
    *
    * @param state The id of the given state.
    * @return The set of ids of the states we can reach.
    */
  private def getNextStates(state: Int): Set[Int] = {
    transitions.filter(t => t.source == state).map(t => t.target).toSet
  }

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
    val next = getNextStates(from)
    next.contains(to)
  }

  /**
    * Checks whether a given "word" of events is accepted by the SDFA.
    *
    * @param events The given "word" of events.
    * @return True if the SDFA accepts the "word".
    */
  override def accepts(events: List[GenericEvent]): Boolean = {
    require(events.nonEmpty)
    val stateReached = getDelta(start, events)
    finals.contains(stateReached)
  }

  /**
    * Finds the state we can reach from a given state with a given "word" of events.
    *
    * @param stateId The id of the given state.
    * @param events The "word" of events.
    * @return The id of the reached state.
    */
  @scala.annotation.tailrec
  private def getDelta(
                        stateId: Int,
                        events: List[GenericEvent]
                      ): Int = {
    require(events.nonEmpty)
    events match {
      case Nil => throw new IllegalArgumentException
      case head :: Nil => getDelta(stateId, head).head
      case head :: tail => {
        val nextState = getDelta(stateId, head).head
        getDelta(nextState, tail)
      }
    }
  }

  /**
    * Finds the state we can reach from a given state with a given event.
    *
    * @param fromStateId The id of the given state.
    * @param withEvent The given event.
    * @return The id of the reached state, provided as a set for compatibility reasons with method of superclass. At
    *         most one state can be reached.
    */
  override def getDelta(
                         fromStateId: Int,
                         withEvent: GenericEvent
                       ): Set[Int] = {
    val relevantTransitions = transitionsMap(fromStateId)
    val destination = getDestination(relevantTransitions, withEvent)
    //TODO: If no enabled transitions are found, we return Set(-1). Probably not the best solution. Maybe better to
    // return empty set.
    Set(destination)
    //getDeltaWithStats(s,e)
  }

  /**
    * From a given list of transitions, finds the one that is enabled with the given event and returns this transition's
    * target state id. Assumes at most one transition can be enabled, as with the outgoing transitions of a SDFA state.
    *
    * @param transitions The list of transitions to check.
    * @param event The given event.
    * @return The id of the target state of the enabled transition.
    */
  @scala.annotation.tailrec
  private def getDestination(
                              transitions: List[Transition],
                              event: GenericEvent
                            ): Int = {
    transitions match {
      //TODO: Make sure that returning -1 in case of no enabled transitions is safe. Typically all state ids are not
      // negative, but check what happens with dead states.
      case Nil => -1
      case head :: tail => {
        val enabled = head.enabled(event)
        if (enabled) head.target else getDestination(tail, event)
      }
    }
  }

  /**
    * Same as fsm.symbolic.sfa.sdfa.SDFA#getDelta(int, stream.GenericEvent).
    * Used only for testing/profiling to check how long the method takes.
    *
    * @param state The id of the given state.
    * @param event The given event.
    * @return The id of the reached state, provided as a set.
    */
  private def getDeltaWithStats(
                                 state: Int,
                                 event: GenericEvent
                               ): Set[Int] = {
    val home = System.getenv("WAYEB_HOME")
    val writer = CSVWriter.open(home + "/results/deltaStats.csv", append = true)
    val t1 = System.nanoTime()
    val relevantTransitions = transitionsMap(state)
    val t2 = System.nanoTime()
    var row: List[String] = List((t2 - t1).toString)
    val t3 = System.nanoTime()
    val destination = getDestinationWithStats(relevantTransitions, event, 0, 0)
    val t4 = System.nanoTime()
    row = (t4 - t3).toString :: row
    row = destination._2.toString :: row
    row = destination._3.toString :: row
    writer.writeRow(row)
    writer.close()
    Set(destination._1)
  }

  /**
    * Same as fsm.symbolic.sfa.sdfa.SDFA#getDestination(scala.collection.immutable.List, stream.GenericEvent).
    * Used only for testing/profiling to check how long the method takes.
    *
    * @param transitions The list of transitions to check.
    * @param event The given event.
    * @param counter Counter to count the number of transitions checked.
    * @param enabledTime Total time to evaluate transitions.
    * @return The id of the target state of the enabled transition, along with the counter and total time.
    */
  @scala.annotation.tailrec
  private def getDestinationWithStats(
                                       transitions: List[Transition],
                                       event: GenericEvent,
                                       counter: Int,
                                       enabledTime: Long
                                     ): (Int, Int, Long) = {
    val home = System.getenv("WAYEB_HOME")
    val writer = CSVWriter.open(home + "/results/enabledStats.csv", append = true)
    transitions match {
      case Nil => {
        writer.close()
        (-1, counter, enabledTime)
      }
      case head :: tail => {
        val t1 = System.nanoTime()
        val enabled = head.enabled(event)
        val t2 = System.nanoTime()
        val diff = t2 - t1
        writer.writeRow(List(head.toStringSentence, diff.toString))
        writer.close()
        if (enabled) (head.target, counter + 1, enabledTime + diff) else getDestinationWithStats(tail, event, counter + 1, enabledTime + diff)
      }
    }
  }

  /**
    * Finds the state we can reach from a given state with a given sentence/minterm.
    *
    * @param fromState The id of the given state.
    * @param withSentence The sentence/minterm to check.
    * @return The state we can reach from the given state with the given sentence.
    */
  def getDeltaWithSentence(
                            fromState: Int,
                            withSentence: Sentence
                          ): Int = {
    val relevantTransitions = transitionsMap(fromState)
    //TODO: Choose which delta on sentence to keep. Normally, for a SDFA, at most one sentence may be found.
    val equippedTransitions = relevantTransitions.find(t => t.equipped(withSentence))
    equippedTransitions match {
      case Some(x) => x.target
      case None => throw new Error("Could not find transition equipped with " + withSentence)
    }
  }

  /**
    * Finds the state we can reach from a given state with a given sentence/minterm.
    * Same as fsm.symbolic.sfa.sdfa.SDFA#getDeltaWithSentence(int, fsm.symbolic.sfa.logic.Sentence), but uses breaks.
    * Possibly a bit faster.
    *
    * @param fromState The id of the given state.
    * @param withSentence The sentence/minterm to check.
    * @return The state we can reach from the given state with the given sentence.
    */
  def getDeltaOnSentence(
                          fromState: Int,
                          withSentence: Sentence
                        ): Int = {
    require(states.contains(fromState), "SDFA\n" + this.toString + "\ndoes not contain state:" + fromState)
    require(this.getSentences.contains(withSentence))
    val transitionsit = transitions.iterator
    var t = fromState
    breakable {
      while (transitionsit.hasNext) {
        val transition = transitionsit.next()
        if ((transition.source == fromState) & (transition.guard.sentence == withSentence)) {
          t = transition.target
          break
        }
      }
    }
    t
  }

  /**
    * Finds the state we can reach from a given state with a given "word" of sentences/minterms.
    *
    * @param fromStateId The id of the given state.
    * @param withSentences The "word" of sentences/minterms to check.
    * @return The state we can reach from the given state with the given sentences.
    */
  def getDeltaOnSentence(
                          fromStateId: Int,
                          withSentences: List[Sentence]
                        ): Int = {
    require(withSentences.nonEmpty)
    withSentences match {
      case Nil => throw new IllegalArgumentException
      case head :: Nil => getDeltaOnSentence(fromStateId, head)
      case head :: tail => {
        val nextState = getDeltaOnSentence(fromStateId, head)
        getDeltaOnSentence(nextState, tail)
      }
    }
  }

  /**
    * Finds the states we can reach from a given state with a given set of sentences/minterms.
    *
    * @param fromState The id of the given state.
    * @param withSentences The set of sentences/minterms to check.
    * @return A set of ids of the states we can reach.
    */
  def getDeltaOnSentenceSet(
                             fromState: Int,
                             withSentences: Set[Sentence]
                           ): Set[Int] = {
    require(withSentences.nonEmpty)
    //a.map(sent => getDeltaOnSentence(s,sent))
    val apar = withSentences.par
    val res = apar.map(sent => getDeltaOnSentence(fromState, sent))
    res.toList.toSet
  }

  /**
    * Checks whether a given state has a loop transition with the given sentence.
    *
    * @param state The given state to check.
    * @param sentence The sentence that the loop transition should have.
    * @return True if the given state has a self-loop transition equipped with the given sentence.
    */
  def isLoopStateOn(
                     state: Int,
                     sentence: Sentence
                   ): Boolean = getDeltaOnSentence(state, sentence) == state

  /**
    * Checks if the given state is accessible from the start state.
    *
    * @param state The given state.
    * @return True if we can reach the given state from the start state.
    */
  def isAccessible(state: Int): Boolean = getAccessibleStatesFrom(start).contains(state)

  /**
    * Returns all states that are accessible from the given state.
    *
    * @param state The given state.
    * @return the ids of all states that can be reached from the given state.
    */
  private def getAccessibleStatesFrom(state: Int): Set[Int] = getAccessibleStatesFromAux(List(state), Set.empty[Int], Set(state))

  @scala.annotation.tailrec
  private def getAccessibleStatesFromAux(
                                          sources: List[Int],
                                          checked: Set[Int],
                                          accessible: Set[Int]
                                        ): Set[Int] = {
    val toCheck = sources.toSet.diff(checked).toList
    toCheck match {
      case Nil => accessible
      case head :: tail => {
        val nextStates = transitions.filter(t => t.source == head).map(t => t.target).toSet
        val newSources = (tail.toSet ++ nextStates).toList
        getAccessibleStatesFromAux(newSources, checked + head, accessible ++ nextStates)
      }
    }
  }

  def getAccessibleStateIDs: Set[Int] = getAccessibleStatesFrom(start)

  override def toString: String = {
    val str = super.toString + "\n" +
      "Duplicates:\n" + duplicates.toString() + "\n" +
      "Deep Duplicates:\n" + getDeepDuplicates.toString() + "\n" +
      "Originals:\n" + findOriginals().toString() + "\n" +
      "Traversal:\n" + traverse().toString()
    str
  }

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

/*************************************************/
  /**  The methods below are not currently in use **/
/*************************************************/

  @deprecated
  private def isAccessibleAux(
                               s: Int,
                               checked: Set[Int]
                             ): Boolean = {
    require(states.contains(s))
    val sources = transitions.filter(t => t.target == s).
      map(t => t.source).
      filter(s => !checked.contains(s))
    if (s == start) true
    else if (sources.isEmpty) false
    else {
      if (sources.contains(start)) true
      else {
        sources.exists(source => isAccessibleAux(source, checked + s))
      }
    }
  }

  @deprecated
  private def buildDeltaOnSentence: Map[(Int, Sentence), Set[Int]] = {
    var deltas = scala.collection.mutable.Map[(Int, Sentence), Set[Int]]()
    val statesit = states.keySet.iterator
    val transitionsit = transitions.iterator
    while (transitionsit.hasNext) {
      val transition = transitionsit.next()
      val sentence = transition.guard.sentence
      val source = transition.source
      val target = transition.target
      if (deltas.isDefinedAt((source, sentence))) {
        deltas((source, sentence)) = deltas((source, sentence)) + target
      } else {
        deltas = deltas + ((source, sentence) -> Set(target))
      }
    }
    deltas.toMap
  }

  @deprecated
  def getDeltaOnSentenceMem(
                             s: Int,
                             sentence: Sentence
                           ): Int = {
    require(states.contains(s), "SDFA\n" + this.toString + "\ndoes not contain state:" + s)
    require(this.getSentences.contains(sentence))
    deltaOnSentence((s, sentence)).head
  }

  @deprecated
  def getDeltaOnSentenceOnlineSca(
                                   s: Int,
                                   sentence: Sentence
                                 ): Int = {
    require(states.contains(s), "SDFA\n" + this.toString + "\ndoes not contain state:" + s)
    require(this.getSentences.contains(sentence))
    val relevantTransitions = transitions.filter(t => t.source == s)
    val enabledTransitions = relevantTransitions.filter(t => t.guard.sentence == sentence)
    val targetStates = enabledTransitions.map(t => t.target)
    val result = targetStates.toSet // remove duplicates
    result.head
  }

}
