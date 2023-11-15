package fsm.symbolic.sfa.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.Sentence
import fsm.symbolic.sfa.{SFAGuard, SFATransition}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object SDFAMutant {
  def apply(sdfa: SDFA): SDFAMutant = new SDFAMutant(sdfa.states, sdfa.transitions, sdfa.start, sdfa.finals, sdfa.duplicates)
}

/**
  * Wrapper class around a SDFA that also contains a representation of the SDFA as a graph. More efficient for various
  * operations on the SDFA, such as those required during disamnbiguation.
  *
  * @param initialStates The states of the original SDFA.
  * @param initialTransitions The transitions of the original SDFA.
  * @param initialStart The start state of the original SDFA.
  * @param initialFinals The final states of the original SDFA.
  * @param initialDuplicates The duplicates of the original SDFA.
  */
class SDFAMutant private[sdfa] (
                                 initialStates: Map[Int, SDFAState],
                                 initialTransitions: List[SFATransition],
                                 initialStart: Int,
                                 initialFinals: Set[Int],
                                 initialDuplicates: Map[Int, Set[Int]]
                               ) extends LazyLogging {

  // First, convert everything to mutable collections. We are going to work incrementally, adding and removing states
  // and transitions.
  val states: mutable.Map[Int, SDFAState] = collection.mutable.HashMap(initialStates.toSeq: _*) //initialStates.to[scala.collection.mutable.Map]
  val transitions: ListBuffer[SFATransition] = initialTransitions.to[ListBuffer]
  val start: Int = initialStart
  val finals: collection.mutable.Set[Int] = initialFinals.to[collection.mutable.Set]
  val duplicates: mutable.Map[Int, Set[Int]] = collection.mutable.HashMap(initialDuplicates.toSeq: _*)

  val sentences: Set[Sentence] = transitions.map(t => t.guard.sentence).toSet
  // now create a more efficient structure for the transitions
  val deltaOnSentence: collection.mutable.HashMap[(Int, Sentence), Set[Int]] = buildDeltaOnSentence
  // and construct the graph
  val graph: SDFAMutantGraph = SDFAMutantGraph(this)

  /**
    * Adds a new state, along with all its outgoing transitions.
    *
    * @param newId The id of the new state.
    * @param newState The new state.
    * @param newTransitions The new outgoing transitions of the state.
    */
  def addState(
                newId: Int,
                newState: SDFAState,
                newTransitions: List[SFATransition]
              ): Unit = {
    logger.whenDebugEnabled {
      require(!states.contains(newId), "State with id :" + newId + " already exists")
      require(addStateCheckSource(newId, newTransitions))
      require(addStateCheckComplete(newTransitions))
      require(addStateCheckTargets(newId, newTransitions))
    }
    states += (newId -> newState)
    transitions ++= newTransitions
    duplicates += (newId -> Set.empty[Int])
    updateDeltaOnSentenceNewTransitions(newId, newTransitions)
    updateGraphNewTransitions(newId, newTransitions)
  }

  /**
    * Checks whether the new transitions all have state as their source.
    *
    * @param state The state to be added.
    * @param transitionsToCheck The transitions to be added.
    * @return True if all transitions have state as their source.
    */
  private def addStateCheckSource(
                                   state: Int,
                                   transitionsToCheck: List[SFATransition]
                                 ): Boolean =
    transitionsToCheck.forall(t => t.source == state)

  /**
    * Checks whether all target states actually exist. Alternatively, if a target state does not exist yet, it should be
    * the same as the source state (which will be created).
    *
    * @param state The state to be added.
    * @param transitionsToCheck The transitions to be added.
    * @return True if all target states exist (or transitions are self-loops).
    */
  private def addStateCheckTargets(
                                    state: Int,
                                    transitionsToCheck: List[SFATransition]
                                  ): Boolean =
    transitionsToCheck.forall(t => (t.target == state) | (states.contains(t.target)))

  /**
    * Checks whether the list of transitions is complete, i.e., whether there exists one transition for every sentence.
    *
    * @param transitionsToCheck The transitions to be added.
    * @return True of the list of transitions is complete.
    */
  private def addStateCheckComplete(transitionsToCheck: List[SFATransition]): Boolean = {
    val theseSentences = transitionsToCheck.map(t => t.guard.sentence).toSet
    theseSentences == sentences
  }

  /**
    * Updates/creates transitions in the delta structure.
    *
    * @param state The id of the new state.
    * @param newTransitions The new transitions to be added/updated.
    */
  private def updateDeltaOnSentenceNewTransitions(
                                                   state: Int,
                                                   newTransitions: List[SFATransition]
                                                 ): Unit = {
    val tit = newTransitions.iterator
    while (tit.hasNext) {
      val t = tit.next()
      val sentence = t.guard.sentence
      val target = t.target
      if (deltaOnSentence.isDefinedAt((state, sentence))) {
        val newTargets = deltaOnSentence((state, sentence)) + target
        deltaOnSentence.update((state, sentence), newTargets)
      } else {
        deltaOnSentence += ((state, sentence) -> Set(target))
      }
    }
  }

  /**
    * Updates the graph of the SDFA with new transitions.
    *
    * @param state The id of the new state.
    * @param newTransitions The new transitions.
    */
  private def updateGraphNewTransitions(
                                         state: Int,
                                         newTransitions: List[SFATransition]
                                       ): Unit = graph.addState(state, newTransitions)

  /**
    * Updates a transition with a new target state.
    *
    * @param source The source state.
    * @param sentence The sentence of the transition.
    * @param newTarget The new target state.
    */
  def updateTransition(
                        source: Int,
                        sentence: Sentence,
                        newTarget: Int
                      ): Unit = {
    val ti = transitions.indexWhere(t => t.source == source & t.guard.sentence == sentence)
    val tb = SFATransition(source, newTarget, SFAGuard(sentence))
    transitions.update(ti, tb)
    updateDeltaOnSentenceOldTransition(source, sentence, newTarget)
    updateGraphOldTransition(source, tb, newTarget)
  }

  /**
    * Updates a transition of the delta structure with a new target state.
    *
    * @param source The source state.
    * @param sentence The sentence of the transition.
    * @param newTarget The new target state.
    */
  private def updateDeltaOnSentenceOldTransition(
                                                  source: Int,
                                                  sentence: Sentence,
                                                  newTarget: Int
                                                ): Unit = deltaOnSentence.update((source, sentence), Set(newTarget))

  /**
    * Updates a transition of the graph with a new target state.
    *
    * @param source The source state.
    * @param transition The sentence of the transition.
    * @param newTarget The new target state.
    */
  private def updateGraphOldTransition(
                                        source: Int,
                                        transition: SFATransition,
                                        newTarget: Int
                                      ): Unit = graph.updateTransition(source, transition, newTarget)

  /**
    * Adds a new duplicate to a state.
    *
    * @param state The state to which we add a new duplicate.
    * @param duplicate The new duplicate.
    */
  def addDuplicate(
                    state: Int,
                    duplicate: Int
                  ): Unit = {
    val newDuplicates = duplicates(state) + duplicate
    duplicates.update(state, newDuplicates)
  }

  /**
    * Sets a state as final.
    *
    * @param newFinal The new final
    */
  def addFinal(newFinal: Int): Unit = finals += newFinal

  /**
    * Retrieves the state we reach from another given state with a given sentence.
    *
    * @param fromState The given state.
    * @param withSentence The given sentence.
    * @return The reached state.
    */
  def getDeltaOnSentence(
                          fromState: Int,
                          withSentence: Sentence
                        ): Int = {
    logger.whenDebugEnabled {
      require(deltaOnSentence.contains((fromState, withSentence)), "SDFA\n" + this.toString + "\ndoes not contain state:" + fromState)
    }
    deltaOnSentence((fromState, withSentence)).head
  }

  /**
    * Finds the state we can reach from a given state with a given "word" of sentences/minterms.
    *
    * @param fromState The id of the given state.
    * @param withSentences The "word" of sentences/minterms to check.
    * @return The state we can reach from the given state with the given sentences.
    */
  def getDeltaOnSentence(
                          fromState: Int,
                          withSentences: List[Sentence]
                        ): Int = {
    logger.whenDebugEnabled {
      require(withSentences.nonEmpty)
    }
    withSentences match {
      case Nil => throw new Error
      case head :: Nil => getDeltaOnSentence(fromState, head)
      case head :: tail => {
        val nextState = getDeltaOnSentence(fromState, head)
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
    withSentences.map(sent => getDeltaOnSentence(fromState, sent))
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
    * Converts a mutant SDFA back to a standard SDFA.
    *
    * @return A standard SDFA equivalent to the mutant SDFA.
    */
  def toSDFA: SDFA = {
    val outStates = states.toMap[Int, SDFAState]
    val outTransitions = transitions.toList
    val outFinals = finals.toSet
    val outDuplicates = duplicates.toMap[Int, Set[Int]]
    SDFA(outStates, outTransitions, start, outFinals, outDuplicates)
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

  /**
    * Returns a sorted list of transitions.
    *
    * @return The list of transitions sorted.
    */
  def getSortedTransitions: List[SFATransition] = transitions.toList.sortWith((t1, t2) => transitionOrder(t1, t2))

  /**
    * Decides the order between two transitions. Order first compares the source states, then the target states and then
    * the sentences.
    *
    * @param t1 The first transition.
    * @param t2 The second transition.
    * @return True if the first transition is first in order.
    */
  private def transitionOrder(
                               t1: SFATransition,
                               t2: SFATransition
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
    * Constructs the delta structure for the transitions.
    *
    * @return The delta structure.
    */
  private def buildDeltaOnSentence: collection.mutable.HashMap[(Int, Sentence), Set[Int]] = {
    var deltas = scala.collection.mutable.Map[(Int, Sentence), Set[Int]]()
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
    val deltasHash = collection.mutable.HashMap(deltas.toSeq: _*)
    deltasHash
  }

  /**
    * Builds Gq = ∆−1 (q) = {p ∈ Q, ∃a ∈ A, δ(p, a) = q}
    *
    * @param stateId The id of the state q.
    * @return Gq.
    */
  def buildGQ(stateId: Int): Set[Int] = graph.getGQ(stateId)

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}
    *
    * @param stateId The id of the state q.
    * @param m The "order".
    * @return Dq
    */
  def buildDQ(
               stateId: Int,
               m: Int
             ): Set[List[Sentence]] = graph.getDQ(stateId, m)

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}.
    * Alternative to fsm.symbolic.sfa.sdfa.SDFAMutant#buildDQ(int, scala.collection.immutable.Set).
    * Does not use the graph. Used for testing.
    *
    * @param stateId The id of the state q.
    * @param Am All permutations of sentences of length m.
    * @return Dq.
    */
  def buildDQ(
               stateId: Int,
               Am: Set[List[Sentence]]
             ): Set[List[Sentence]] = {
    //val Am = utils.SetUtils.permutationsAlt(sentences, m)(m)
    if (Am.isEmpty) Set.empty[List[Sentence]]
    else {
      val statesIds = states.keySet
      var AmFiltered = Set.empty[List[Sentence]]
      val Amit = Am.iterator
      while (Amit.hasNext) {
        val a = Amit.next()
        var existsToq = false
        breakable {
          val statesit = statesIds.iterator
          while (statesit.hasNext) {
            val id = statesit.next()
            if (getDeltaOnSentence(id, a) == stateId) {
              existsToq = true
              break
            }
          }
        }
        if (existsToq) {
          AmFiltered = AmFiltered + a
        }
      }
      AmFiltered
    }
  }

}
