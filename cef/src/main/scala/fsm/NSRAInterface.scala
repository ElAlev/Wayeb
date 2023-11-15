package fsm

import breeze.linalg.DenseMatrix
import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sra.Configuration
import fsm.symbolic.sra.nsra.NSRA
import model.markov.TransitionProbs
import model.vmm.pst.CyclicBuffer
import stream.GenericEvent
import ui.ConfigUtils

object NSRAInterface {
  /**
   * Constructor for NSRA interface.
   *
   * @param nsra               The symbolic non-deterministic finite automaton with registers (NSRA) to wrap.
   * @param id                 The NSRA's unique id.
   * @param partitionAttribute The partition attribute of the NSRA's pattern.
   * @param window             The NSRA's window.
   * @param windowType         The NSRA's window type (count or time).
   * @return The NSRA interface.
   */
  def apply(
             nsra: NSRA,
             id: Int,
             partitionAttribute: String,
             window: Int,
             windowTypeStr: String
           ): NSRAInterface = new NSRAInterface(nsra, id, partitionAttribute, window, WindowType.str2Wt(windowTypeStr))

  /**
   * Constructor for NSRA interface.
   *
   * @param nsra               The symbolic non-deterministic finite automaton with registers (NSRA) to wrap.
   * @param id                 The NSRA's unique id.
   * @param partitionAttribute The partition attribute of the NSRA's pattern.
   * @param window             The NSRA's window.
   * @return The NSRA interface.
   */
  def apply(
             nsra: NSRA,
             id: Int,
             partitionAttribute: String,
             window: Int
           ): NSRAInterface = new NSRAInterface(nsra, id, partitionAttribute, window, COUNT)

  /**
   * Constructor for NSRA interface.
   *
   * @param nsra The symbolic non-deterministic finite automaton with registers (NSRA) to wrap.
   * @return The NSRA interface.
   */
  def apply(nsra: NSRA): NSRAInterface = new NSRAInterface(nsra, id = 1, ConfigUtils.singlePartitionVal, window = 0, windowType = COUNT)
}

/**
 * This class is a wrapper for non-deterministic symbolic automata with registers. Extends fsm.FSMInterface.
 *
 * @param nsra The symbolic deterministic finite automaton with registers (NSRA) to wrap.
 * @param id The NSRA's unique id.
 * @param partitionAttribute The partition attribute of the NSRA's pattern.
 * @param window The NSRA's window.
 */
class NSRAInterface(
                     val nsra: NSRA,
                     val id: Int,
                     val partitionAttribute: String,
                     val window: Int,
                     val windowType: WindowType
                   ) extends FSMInterface with Serializable {
  /**
   * @return The states of the FSM.
   */
  override def getStates: Set[Int] = nsra.states.keySet

  /**
   * @return The start state.
   */
  override def getStartId: Int = nsra.start

  /**
   * @return The final states.
   */
  override def getFinals: Set[Int] = nsra.finals

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

  override def getRuntimeWindow: Int = window

  override def getWindowType: WindowType = windowType

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
   * @param conf         The current configuration (state + valuation) of the automaton.
   * @return The next state that the FSM moves to.
   */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent,
                             buffer: CyclicBuffer,
                             conf: Configuration
                           ): Set[Configuration] = nsra.yieldsSuccessorConfigNoEpsilon(conf, event)

  /**
   * Serializes the FSM and writes it to a file.
   *
   * @param fn The path to the file.
   */
  override def write2File(fn: String): Unit = nsra.write2File(fn)

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
  override def connected(from: Int, to: Int): Boolean = nsra.connected(from, to)

}
