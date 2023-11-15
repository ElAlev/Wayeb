package fsm

import breeze.linalg.DenseMatrix
import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sra.Configuration
import fsm.symbolic.sra.dsra.DSRAStreaming
import model.markov.TransitionProbs
import model.vmm.pst.CyclicBuffer
import stream.GenericEvent
import ui.ConfigUtils

object DSRAInterface {
  /**
   * Constructor for DSRA interface.
   *
   * @param dsra               The symbolic deterministic finite automaton with registers (DSRA) to wrap.
   * @param id                 The DSRA's unique id.
   * @param partitionAttribute The partition attribute of the DSRA's pattern.
   * @return                   The DSRA interface.
   */
  def apply(
             dsra: DSRAStreaming,
             id: Int,
             partitionAttribute: String
           ): DSRAInterface = new DSRAInterface(dsra, id, partitionAttribute)

  /**
   *
   * Constructor for DSRA interface with default partition attribute (none).
   *
   * @param dsra The symbolic deterministic finite automaton with registers (DSRA) to wrap.
   * @return     The DSRA interface.
   */
  def apply(
             dsra: DSRAStreaming,
             id: Int
           ): DSRAInterface = new DSRAInterface(dsra, id, ConfigUtils.singlePartitionVal)

  /**
   *
   * Constructor for DSRA interface with default id (1) and partition attribute (none).
   *
   * @param dsra The symbolic deterministic finite automaton with registers (DSRA) to wrap.
   * @return     The DSRA interface.
   */
  def apply(dsra: DSRAStreaming): DSRAInterface = new DSRAInterface(dsra, id = 1, ConfigUtils.singlePartitionVal)
}

/**
 * This class is a wrapper for deterministic symbolic automata with registers. Extends fsm.FSMInterface.
 *
 * @param dsra               The (streaming) symbolic deterministic finite automaton with registers (DSRA) to wrap.
 * @param id                 The DSRA's unique id.
 * @param partitionAttribute The partition attribute of the DSRA's pattern.
 */
class DSRAInterface(
                     val dsra: DSRAStreaming,
                     val id: Int,
                     val partitionAttribute: String
                   ) extends FSMInterface with Serializable {

  /**
   * @return The states of the FSM.
   */
  override def getStates: Set[Int] = dsra.hyperStates.toSet

  /**
   * @return The start state.
   */
  override def getStartId: Int = dsra.start

  /**
   * @return The final states.
   */
  override def getFinals: Set[Int] = dsra.finals

  /**
   * Each FSM has a unique ID.
   *
   * @return The FSM's ID.
   */
  override def getId: Int = id

  /**
   * Whenever a new event contains a new value for the FSM's partition attribute, a new run is spawned.
   *
   * @return The partition attribute
   */
  override def getPartitionAttribute: String = partitionAttribute

  override def getRuntimeWindow: Int = 0

  override def getWindowType: WindowType = COUNT

  /**
   * Given a (current) state of the FSM and a new input event, find the next state. Buffer used only with SPSTs. In
   * this case, the buffer is used to find the leaf of the tree, See relevant implementation in
   * fsm.SPSTInterface#getNextState(int, stream.GenericEvent, vmm.pst.CyclicBuffer).
   * In all other cases, the buffer argument is ignored.
   * Configuration used only with SRA. In all other cases, it is ignored.
   *
   * @param currentState The FSM's current state.
   * @param event        The new input event.
   * @param buffer       The buffer holding the current context/suffix.
   * @param currentConf  The current configuration (state + valuation) of the automaton.
   * @return             The next state that the FSM moves to.
   */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent,
                             buffer: CyclicBuffer,
                             currentConf: Configuration
                           ): Set[Configuration] = dsra.yieldsSuccessorConfig(currentConf, event)

  /**
   * Serializes the FSM and writes it to a file.
   *
   * @param fn The path to the file.
   */
  override def write2File(fn: String): Unit = dsra.write2File(fn)

  /**
   * Creates a Markov chain transition matrix from a set of conditional probabilities.
   * Used for testing, only by fsm.DFAInterface. For other FSMs, just creates a dummy matrix.
   *
   * @param probs The set of conditional probabilities.
   * @return A transition matrix corresponding to the given conditional probabilities.
   */
  override def buildTransitionMatrix(probs: TransitionProbs): DenseMatrix[Double] = new DenseMatrix[Double](0, 0)

  /**
   * Checks whether there is a transition from a given state to another.
   *
   * @param from The candidate source state.
   * @param to   The candidate target state.
   * @return Return true if there is indeed a transition.
   */
  override def connected(from: Int, to: Int): Boolean = dsra.connected(from, to)
}
