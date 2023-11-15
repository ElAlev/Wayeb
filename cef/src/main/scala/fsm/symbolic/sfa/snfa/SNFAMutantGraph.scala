package fsm.symbolic.sfa.snfa

import fsm.symbolic.logic.{Predicate, Sentence}
import scala.collection.mutable

object SNFAMutantGraph {
  /**
    * Constructor for SNFAMutantGraph.
    *
    * @param snfasm The root state of the SNFA. Typically, it should be the SNFA's start state.
    * @param accessor A hash map structure (from state ids to state objects) providing direct access to the states. Useful
    *                 when we want to access a certain state but want to avoid starting from the start/root state and
    *                 searching for it.
    * @param finals The ids of the final states.
    * @return The SNFAMutantGraph.
    */
  def apply(
             snfasm: SNFAStateMutant,
             accessor: mutable.HashMap[Int, SNFAStateMutant],
             finals: Set[Int]
           ): SNFAMutantGraph = new SNFAMutantGraph(snfasm, accessor, finals)

  /**
    * Constructor for SNFAMutantGraph.
    *
    * @param snfa The original SNFA.
    * @return The transformed SNFA.
    */
  def apply(snfa: SNFA): SNFAMutantGraph = {
    val (snfasm, accessor) = buildGraph(snfa)
    apply(snfasm, accessor, snfa.finals)
  }

  /**
    * Transforms a SNFA to a graph SNFA.
    *
    * @param snfa The original SNFA.
    * @return The graph SNFA.
    */
  private def buildGraph(snfa: SNFA): (SNFAStateMutant, mutable.HashMap[Int, SNFAStateMutant]) = {
    val transitions = snfa.transitions
    val frontStates = collection.mutable.Set[Int](snfa.start)
    val snfasm = SNFAStateMutant(snfa.start)
    val created = collection.mutable.HashMap[Int, SNFAStateMutant](snfa.start -> snfasm)
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
          val new_snfasm = SNFAStateMutant(target)
          created += (target -> new_snfasm)
          frontStates += target
          stateToExpand.addNext((new_snfasm, rt))
          new_snfasm.addPrevious((stateToExpand, rt))
        } else {
          val existing_snfasm = created(target)
          stateToExpand.addNext((existing_snfasm, rt))
          existing_snfasm.addPrevious((stateToExpand, rt))
        }
      }
      frontStates -= stateToExpandId
    }
    (snfasm, created)

  }
}

/**
  * Class representing a SNFA as a graph, where each state has direct references to its next and previous states.
  * Useful for optimizing certain operations on a SNFA.
  *
  * @param snfasm The root state of the SNFA. Typically, it should be the SNFA's start state.
  * @param accessor A hash map structure (from state ids to state objects) providing direct access to the states. Useful
  *                 when we want to access a certain state but want to avoid starting from the start/root state and
  *                 searching for it.
  * @param finals The ids of the final states.
  */
class SNFAMutantGraph(
                       val snfasm: SNFAStateMutant,
                       val accessor: collection.mutable.HashMap[Int, SNFAStateMutant],
                       val finals: Set[Int]
                     ) extends Serializable {

  /**
    * Finds all the next states (successors) of a certain state, given a sentence and a set of exclusives, i.e.,
    * a next state is included only if the guard of the transition to it is triggered whenever the given sentence
    * is true (taking the exclusives into account).
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
    val state = accessor(stateId)
    // This is the benefit of using a graph. We can directly retrieve all the next states.
    val next = state.getNext
    val relevantTransitions = next.map(st => st._2)
    // keep only those transitions which would be triggered if the given sentence is true
    val predTransitions = relevantTransitions.filter(t => !t.isEpsilon & sentence.entails(t.guard.sentence, exclusives))
    val targetStates = predTransitions.map(t => t.target)
    targetStates.toSet
  }

  /**
    * Finds all the next states that can be accessed from the given state by following only epsilon transitions.
    *
    * @param stateId The id of the given state for which we want to find its epsilon targets.
    * @return The ids of the epsilon targets.
    */
  def getEpsilonDelta(stateId: Int): Set[Int] = {
    val state = accessor(stateId)
    val next = state.getNext
    val relevantTransitions = next.map(st => st._2)
    val enabledTransitions = relevantTransitions.filter(t => t.isEpsilon)
    val targetStates = enabledTransitions.map(t => t.target)
    targetStates.toSet
  }

}
