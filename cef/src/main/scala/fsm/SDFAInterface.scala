package fsm

import breeze.linalg.DenseMatrix
import fsm.symbolic.sfa.logic.Sentence
import fsm.symbolic.sfa.sdfa.SDFA
import model.markov.TransitionProbs
import stream.GenericEvent
import ui.ConfigUtils

object SDFAInterface {
  /**
    * Constructor for SDFA interface with default id (1) and partition attribute (none).
    *
    * @param sdfa The symbolic deterministic finite automaton (SDFA) to wrap.
    * @return The SDFA interface.
    */
  def apply(sdfa: SDFA): SDFAInterface = new SDFAInterface(sdfa, 1, ConfigUtils.singlePartitionVal)

  /**
    * Constructor for SDFA interface with default partition attribute (none).
    *
    * @param sdfa The symbolic deterministic finite automaton (SDFA) to wrap.
    * @param id The SDFA's unique id.
    * @return The SDFA interface.
    */
  def apply(
             sdfa: SDFA,
             id: Int
           ): SDFAInterface = new SDFAInterface(sdfa, id, ConfigUtils.singlePartitionVal)

  /**
    * Constructor for SDFA interface.
    *
    * @param sdfa The symbolic deterministic finite automaton (SDFA) to wrap.
    * @param id The SDFA's unique id.
    * @param partitionAttribute The partition attribute of the SDFA's pattern.
    * @return The SDFA interface.
    */
  def apply(
             sdfa: SDFA,
             id: Int,
             partitionAttribute: String
           ): SDFAInterface = new SDFAInterface(sdfa, id, partitionAttribute)
}

/**
  * This class is a wrapper for symbolic automata. Extends fsm.FSMInterface.
  *
  * @param sdfa The symbolic deterministic finite automaton (SDFA) to wrap.
  * @param id The SDFA's unique id.
  * @param partitionAttribute The partition attribute of the SDFA's pattern.
  */
class SDFAInterface(
                     val sdfa: SDFA,
                     val id: Int,
                     val partitionAttribute: String
                   ) extends FSMInterface with Serializable {
  /**
    * @return The final states.
    */
  override def getFinals: Set[Int] = sdfa.finals

  /**
    * @return The non-final states.
    */
  override def getNonFinals: Set[Int] = sdfa.states.keySet &~ sdfa.finals

  /**
    * @return The states of the FSM.
    */
  override def getStates: Set[Int] = sdfa.states.keySet

  /**
    * @return The start state.
    */
  override def getStartId: Int = sdfa.start

  /**
    * @return The FSM's ID.
    */
  override def getId: Int = id

  /**
    * Given a (current) state of the FSM and a new input event, find the next state.
    *
    * @param currentState The FSM's current state.
    * @param event The new input event.
    * @return The next state that the FSM moves to.
    */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent
                           ): Int =
    sdfa.getDelta(currentState, event).head

  /**
    * Finds the target state from a source state, given a minterm/sentence.
    *
    * @param fromState The source state.
    * @param withSentence The minterm.
    * @return The target state,
    */
  def getNextStateWithSentence(
                                fromState: Int,
                                withSentence: Sentence
                              ): Int = sdfa.getDeltaWithSentence(fromState, withSentence)

  /**
    * Finds the sequence of states followed from a source state, given a sequence of minterms/sentences.
    *
    * @param fromState The source state.
    * @param withSentences The sequence/list of minterms.
    * @return The sequence of target states,
    */
  def getNextStateWithSentences(
                                 fromState: Int,
                                 withSentences: List[Sentence]
                               ): List[Int] = getNextStateWithSentencesAux(fromState, withSentences, List.empty)

  /**
    * Recursively finds the sequence of states followed from a source state, given a sequence of minterms/sentences.
    *
    * @param fromState The source state,
    * @param withSentences The sequence/list of minterms.
    * @param accumulator Holds the current sequence of states during recursive calls.
    * @return The sequence of target states,
    */
  @scala.annotation.tailrec
  private def getNextStateWithSentencesAux(
                                            fromState: Int,
                                            withSentences: List[Sentence],
                                            accumulator: List[Int]
                                          ): List[Int] = {
    withSentences match {
      case Nil => accumulator.reverse
      case head :: tail => {
        val newState = getNextStateWithSentence(fromState, head)
        val newAccumulator = newState :: accumulator
        getNextStateWithSentencesAux(newState, tail, newAccumulator)
      }
    }
  }

  /**
    * Serializes SDFA and writes it to a file.
    *
    * @param fn The path to the file.
    */
  override def write2File(fn: String): Unit = sdfa.write2File(fn)

  /**
    * @return The partition attribute
    */
  override def getPartitionAttribute: String = partitionAttribute

  /**
    * Actually does nothing here, just creates a dummy matrix.
    *
    * @param probs The set of conditional probabilities.
    * @return A transition matrix corresponding to the given conditional probabilities.
    */
  override def buildTransitionMatrix(probs: TransitionProbs): DenseMatrix[Double] = new DenseMatrix[Double](0, 0)

  /**
    * Checks whether there is a transition from a given state to another.
    *
    * @param from The candidate source state.
    * @param to The candidate target state.
    * @return Return true if there is indeed a transition.
    */
  override def connected(
                          from: Int,
                          to: Int
                        ): Boolean = {
    sdfa.connected(from, to)
  }

  override def toString: String = sdfa.toString

}
