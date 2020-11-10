package fsm

import breeze.linalg.DenseMatrix
import fsm.classical.fa.dfa.DFA
import model.markov.TransitionProbs
import stream.GenericEvent
import ui.ConfigUtils

object DFAInterface {
  /**
    * Constructor for DFA interface with default id (1) and partition attribute (none).
    *
    * @param dfa The deterministic automaton to wrap.
    * @return The DFA interface.
    */
  def apply(dfa: DFA): DFAInterface = new DFAInterface(dfa, 1, ConfigUtils.singlePartitionVal)

  /**
    * Constructor for DFA interface with default partition attribute (none).
    *
    * @param dfa The deterministic automaton to wrap.
    * @param id The DFA's unique id.
    * @return The DFA interface.
    */
  def apply(
             dfa: DFA,
             id: Int
           ): DFAInterface = new DFAInterface(dfa, id, ConfigUtils.singlePartitionVal)

  /**
    * Constructor for DFA interface.
    *
    * @param dfa The deterministic automaton to wrap.
    * @param id The DFA's unique id.
    * @param partitionAttribute The partition attribute of the DFA's pattern.
    * @return The DFA interface.
    */
  def apply(
             dfa: DFA,
             id: Int,
             partitionAttribute: String
           ): DFAInterface = new DFAInterface(dfa, id, partitionAttribute)
}

/**
  * This class is a wrapper for classical automata. Extends fsm.FSMInterface.
  *
  * @param dfa The deterministic automaton to wrap.
  * @param id The DFA's unique id.
  * @param partitionAttribute The partition attribute of the DFA's pattern.
  */
class DFAInterface(
                    val dfa: DFA,
                    val id: Int,
                    val partitionAttribute: String
                  ) extends FSMInterface with Serializable {

  /**
    * @return The final states.
    */
  override def getFinals: Set[Int] = dfa.getAllFinals.toSet

  /**
    * @return The non-final states.
    */
  override def getNonFinals: Set[Int] = dfa.getAllNonFinals.toSet

  /**
    * @return The states of the FSM.
    */
  override def getStates: Set[Int] = dfa.getStateKeys.toSet

  /**
    * @return The start state.
    */
  override def getStartId: Int = dfa.getStart

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
                           ): Int = dfa.delta(currentState, event.eventType)

  /**
    * Serialized DFA and writes it to a file.
    *
    * @param fn The path to the file.
    */
  override def write2File(fn: String): Unit = dfa.write2File(fn)

  /**
    * @return The partition attribute
    */
  override def getPartitionAttribute: String = partitionAttribute

  /**
    * Creates a Markov chain transition matrix from a set of conditional probabilities.
    *
    * @param probs The set of conditional probabilities.
    * @return A transition matrix corresponding to the given conditional probabilities.
    */
  override def buildTransitionMatrix(probs: TransitionProbs): DenseMatrix[Double] = {
    val state2Row = buildState2Row
    val size = dfa.size
    val m = new DenseMatrix[Double](size, size)
    val inputSymbols = dfa.getInputSymbols
    val states = dfa.getStates
    // loop through all states, setting each as a source state
    for (statei <- 0 until size) {
      val state = states(statei)
      val fromRow = state2Row(statei)
      // for each input symbol, find the target state and increase the transition probability
      // note that we might get the same target state for multiple symbols and we we have to add all these probabilities
      for (is <- inputSymbols) {
        val toi = state.getDelta(is)
        val toRow = state2Row(toi)
        var transProb = 0.0
        if (state.isStart & state.getLabel.isEmpty) transProb = probs.getMarginal(is)
        else transProb = probs.getProb(state.getLabel, is)
        m(fromRow, toRow) += transProb
      }
    }
    // we make all final states sinks
    val finals = dfa.getAllFinals
    for (f <- finals) {
      val r = state2Row(f)
      for (i <- 0 until size) {
        if (i == r) m(r, i) = 1.0
        else m(r, i) = 0.0
      }
    }
    m
  }

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
                        ): Boolean = dfa.connected(from, to)

  override def toString: String = dfa.printDFA

}
