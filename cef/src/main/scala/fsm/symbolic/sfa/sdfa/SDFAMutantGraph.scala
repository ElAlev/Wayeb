package fsm.symbolic.sfa.sdfa

import fsm.symbolic.sfa.Transition
import fsm.symbolic.sfa.logic.Sentence
import stream.GenericEvent
import scala.collection.mutable

object SDFAMutantGraph {
  /**
    * Constructor for SDFAMutantGraph.
    *
    * @param sdfasm   The root state of the SDFA. Typically, it should be the SDFA's start state.
    * @param accessor A hash map structure (from state ids to state objects) providing direct access to the states.
    *                 Useful when we want to access a certain state but want to avoid starting from the start/root state
    *                 and searching for it.
    * @param finals The ids of the final states.
    * @return The SDFAMutantGraph.
    */
  def apply(
             sdfasm: SDFAStateMutant,
             accessor: mutable.HashMap[Int, SDFAStateMutant],
             finals: Set[Int]
           ): SDFAMutantGraph = new SDFAMutantGraph(sdfasm, accessor, finals)

  /**
    * Constructor for SDFAMutantGraph.
    *
    * @param sdfam The SDFA mutant.
    * @return The transformed SDFA mutant as SDFAMutantGraph.
    */
  def apply(sdfam: SDFAMutant): SDFAMutantGraph = {
    val (sdfasm, accessor) = buildGraph(sdfam)
    apply(sdfasm, accessor, sdfam.finals.toSet)
  }

  /**
    * Transforms a mutant SDFA to a graph SDFA.
    *
    * @param sdfam The original mutant SDFA.
    * @return The graph SDFA.
    */
  private def buildGraph(sdfam: SDFAMutant): (SDFAStateMutant, mutable.HashMap[Int, SDFAStateMutant]) = {
    val transitions = sdfam.transitions
    val frontStates = collection.mutable.Set[Int](sdfam.start)
    val sdfasm = SDFAStateMutant(sdfam.start)
    val created = collection.mutable.HashMap[Int, SDFAStateMutant](sdfam.start -> sdfasm)
    // States are created incrementally, beginning with the start state. For each unchecked state,
    // we find all its next states and add them as references. If a next state has not been previously checked,
    // we also create it.
    while (frontStates.nonEmpty) {
      val stateToExpandId = frontStates.head
      val stateToExpand = created(stateToExpandId)
      val relevantTransitions = transitions.filter(t => t.source == stateToExpandId).iterator
      while (relevantTransitions.hasNext) {
        val rt = relevantTransitions.next()
        val target = rt.target
        if (!created.contains(target)) {
          val new_sdfasm = SDFAStateMutant(target)
          created += (target -> new_sdfasm)
          frontStates += target
          stateToExpand.addNext((new_sdfasm, rt))
          new_sdfasm.addPrevious((stateToExpand, rt))
        } else {
          val existing_sdfasm = created(target)
          stateToExpand.addNext((existing_sdfasm, rt))
          existing_sdfasm.addPrevious((stateToExpand, rt))
        }
      }
      frontStates -= stateToExpandId
    }
    (sdfasm, created)
  }
}

/**
  * Class representing a SDFA as a graph, where each state has direct references to its next and previous states.
  * Useful for optimizing certain operations on a SDFA.
  *
  * @param sdfasm   The root state of the SDFA. Typically, it should be the SDFA's start state.
  * @param accessor A hash map structure (from state ids to state objects) providing direct access to the states. Useful
  *                 when we want to access a certain state but want to avoid starting from the start/root state and
  *                 searching for it.
  * @param finals The ids of the final states.
  */
class SDFAMutantGraph(
                       val sdfasm: SDFAStateMutant,
                       val accessor: collection.mutable.HashMap[Int, SDFAStateMutant],
                       val finals: Set[Int]
                     ) {

  /**
    * Adds a new state, along with all its outgoing transitions.
    *
    * @param newStateId The id of the new state.
    * @param newTransitions The new transitions.
    */
  def addState(
                newStateId: Int,
                newTransitions: List[Transition]
              ): Unit = {
    val newState = SDFAStateMutant(newStateId)
    accessor += (newStateId -> newState)
    val tlit = newTransitions.iterator
    while (tlit.hasNext) {
      val transition = tlit.next()
      val target = transition.target
      val targetState = accessor(target)
      newState.addNext((targetState, transition))
      targetState.addPrevious((newState, transition))
    }
  }

  /**
    * Updates a transition of the graph with a new target state.
    *
    * @param source The source state.
    * @param transition The sentence of the transition.
    * @param newTarget The new target state.
    */
  def updateTransition(
                        source: Int,
                        transition: Transition,
                        newTarget: Int
                      ): Unit = {
    val sourceState = accessor(source)
    val newTargetState = accessor(newTarget)
    val oldTargetStateSet = sourceState.getNext(transition.guard.sentence)
    if (oldTargetStateSet.nonEmpty) {
      // Assumes automaton is deterministic, i.e., from sourceState at most one target state with the same sentence
      val oldTargetState = oldTargetStateSet.head
      sourceState.removeNext(oldTargetState, transition.guard.sentence)
      oldTargetState.removePrevious(sourceState, transition.guard.sentence)
    }
    sourceState.addNext((newTargetState, transition))
    newTargetState.addPrevious((sourceState, transition))
  }

  /**
    * Checks whether a given "word" of events is accepted by the SDFA.
    *
    * @param events The given "word" of events.
    * @return True if the SDFA accepts the "word".
    */
  def accepts(events: List[GenericEvent]): Boolean = finals.contains(getDelta(sdfasm.id, events))

  /**
    * Finds the state we can reach from a given state with a given event.
    *
    * @param stateId The id of the given state.
    * @param event The given event.
    * @return The id of the reached state, provided as a set.
    */
  def getDelta(
                stateId: Int,
                event: GenericEvent
              ): Set[Int] = {
    require(accessor.contains(stateId))
    val state = accessor(stateId)
    val next = state.getNext
    val dest = next.filter(el => el._2.enabled(event)).map(el1 => el1._1.id)
    dest.toSet
  }

  /**
    * Finds the state we can reach from a given state with a given "word" of events.
    *
    * @param stateId The id of the given state.
    * @param events The "word" of events.
    * @return The id of the reached state.
    */
  def getDelta(
                stateId: Int,
                events: List[GenericEvent]
              ): Int = {
    require(accessor.contains(stateId))
    val state = accessor(stateId)
    val dest = getDeltaAux(state, events)
    dest.id
  }

  /**
    * Recursive method to find the state we can reach from a given state with a given "word" of events.
    *
    * @param state The current state.
    * @param events The remaining events of the "word".
    * @return The reached state.
    */
  def getDeltaAux(
                   state: SDFAStateMutant,
                   events: List[GenericEvent]
                 ): SDFAStateMutant = {
    events match {
      case Nil => state
      case head :: tail => {
        val next = state.getNext
        val dest = next.filter(el => el._2.enabled(head)).map(el1 => el1._1).head
        getDeltaAux(dest, tail)
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
  def getDeltaOnSentence(
                          fromState: Int,
                          withSentence: Sentence
                        ): Int = {
    require(accessor.contains(fromState))
    val state = accessor(fromState)
    val next = state.getNext
    next.filter(el => el._2.guard.sentence == withSentence).map(el1 => el1._1.id).head
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
    withSentences match {
      case Nil => fromState
      case head :: tail => {
        val next = getDeltaOnSentence(fromState, head)
        getDeltaOnSentence(next, tail)
      }
    }
  }

  /**
    * Builds Gq = ∆−1 (q) = {p ∈ Q, ∃a ∈ A, δ(p, a) = q}
    *
    * @param stateId The id of the state q.
    * @return Gq
    */
  def getGQ(stateId: Int): Set[Int] = {
    val state = accessor(stateId)
    val previous = state.getPrevious
    val previousIds = previous.map(p => p._1.id)
    previousIds.toSet
  }

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}
    *
    * @param stateId The id of the state q.
    * @param m The "order".
    * @return Dq.
    */
  def getDQ(
             stateId: Int,
             m: Int
           ): Set[List[Sentence]] = {
    require(m > 0)
    require(accessor.contains(stateId))
    val state = accessor(stateId)
    getDQAux(state, m, Nil)
  }

  def getDQAux(
                q: SDFAStateMutant,
                m: Int,
                breadcrumbs: List[Sentence]
              ): Set[List[Sentence]] = {
    if (m == 0) Set(breadcrumbs)
    else {
      val previousit = q.getPrevious.iterator
      val gathered = mutable.Set[List[Sentence]]()
      while (previousit.hasNext) {
        val previous = previousit.next()
        val previous_crumb = previous._2.guard.sentence
        val new_breadcrumbs = previous_crumb :: breadcrumbs
        val previous_q = previous._1
        val gatheredFromq = getDQAux(previous_q, m - 1, new_breadcrumbs)
        gathered ++= gatheredFromq
      }
      gathered.toSet
    }

  }

}
