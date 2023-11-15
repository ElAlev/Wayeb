package fsm

import breeze.linalg.DenseMatrix
import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.symbolic.sra.Configuration
import model.markov.{MarkovChain, MarkovChainFactory, TransitionProbs}
import stream.GenericEvent
import model.vmm.SymbolWord
import model.vmm.mapper.Isomorphism
import model.vmm.pst.CyclicBuffer
import model.vmm.pst.spsa.SymbolicPSA
import ui.ConfigUtils

object SPSAInterface {
  /**
    * Constructor for SPSA interface.
    *
    * @param sdfa The SDFA extracted from the SPSA. Note that we no longer need the transition probabilities of the SPSA.
    *             The should be encoded in the transition matrix.
    * @param label2id Mapping of SPSA labels to SDFA ids.
    * @param id2label Mapping of SDFA ids to SPSA labels.
    * @param state2row Mapping of SDFA states to matrix rows.
    * @param transitionMatrix The transition matrix.
    * @param iso The isomoprhism mapping SDFA minterms to symbols.
    * @param startStates The set of start states.
    * @param maxOrder The maximum order of the underlying SPSA.
    * @param id The SDFA's unique id.
    * @param partitionAttribute The partition attribute of the SDFA's pattern.
    * @return The SPSA interface.
    */
  def apply(
             sdfa: SDFA,
             label2id: Map[(Int, SymbolWord), Int],
             id2label: Map[Int, (Int, SymbolWord)],
             state2row: Map[Int, Int],
             transitionMatrix: DenseMatrix[Double],
             iso: Isomorphism,
             startStates: Set[(Int, SymbolWord)],
             maxOrder: Int,
             id: Int,
             partitionAttribute: String
           ): SPSAInterface = new SPSAInterface(sdfa, label2id, id2label, state2row, transitionMatrix, iso, startStates, maxOrder, id, partitionAttribute)

  /**
    * Constructor for SPSA interface.
    *
    * @param spsa The SPSA.
    * @param id The SDFA's unique id.
    * @param partitionAttribute The partition attribute of the SDFA's pattern.
    * @return The SPSA interface.
    */
  def apply(
             spsa: SymbolicPSA,
             id: Int,
             partitionAttribute: String
           ): SPSAInterface = {
    createSPSAInterface(spsa, id, partitionAttribute)
  }

  /**
    * Constructor for SPSA interface.
    *
    * @param spsa The SPSA.
    * @param id The SDFA's unique id.
    * @return The SPSA interface.
    */
  def apply(
             spsa: SymbolicPSA,
             id: Int
           ): SPSAInterface = {
    createSPSAInterface(spsa, id, ConfigUtils.singlePartitionVal)
  }

  /**
    * Creates a SPSA wrapper from a SPSA.
    *
    * @param spsa The SPSA.
    * @param id The SPSA's unique id.
    * @param partitionAttribute The partition attribute of the SDFA's pattern.
    * @return The wrapper.
    */
  private def createSPSAInterface(
                                   spsa: SymbolicPSA,
                                   id: Int,
                                   partitionAttribute: String
                                 ): SPSAInterface = {
    val (sdfa, _, label2id, id2label) = spsa.toSDFA
    val sdfai = SDFAInterface(sdfa)
    val s2r = sdfai.buildState2Row
    val tm = buildTransitionMatrix(spsa, s2r, id2label)
    val iso = spsa.iso
    val ss = spsa.getStartStates
    val mo = spsa.maxOrder
    val spsai = new SPSAInterface(sdfa, label2id, id2label, s2r, tm, iso, ss, mo, id, partitionAttribute)
    spsai
  }

  /**
    * Creates the transition matrix of a SPSA. This is essentially the transition matrix of the extracted SDFA.
    *
    * @param spsa The SPSA.
    * @param state2row Mapping of SDFA states to metrix rows.
    * @param id2label Mapping of SDFA ids to SPSA labels.
    * @return The transition matrix.
    */
  private def buildTransitionMatrix(
                                     spsa: SymbolicPSA,
                                     state2row: Map[Int, Int],
                                     id2label: Map[Int, (Int, SymbolWord)]
                                   ): DenseMatrix[Double] = {
    val size = spsa.getSize
    val matrix = new DenseMatrix[Double](size, size)
    for (iState <- 1 to size) {
      val iRow = state2row(iState)
      for (jState <- 1 to size) {
        val jRow = state2row(jState)
        matrix(iRow, jRow) = getProbFromTo(spsa, id2label, iState, jState)
      }
    }
    matrix
  }

  private def getProbFromTo(
                             spsa: SymbolicPSA,
                             id2label: Map[Int, (Int, SymbolWord)],
                             fromState: Int,
                             toState: Int
                           ): Double = {
    val fromLabel = id2label(fromState)
    val toLabel = id2label(toState)
    spsa.getProbFromTo(fromLabel, toLabel)
  }
}

/**
  * This class is a wrapper for symbolic probabilistic suffix automata (SPSA), i.e., embeddings.
  * Extends fsm.FSMInterface.
  *
  * @param sdfa The SDFA extracted from the SPSA. Note that we no longer need the transition probabilities of the SPSA.
  *             The should be encoded in the transition matrix.
  * @param label2id Mapping of SPSA labels to SDFA ids.
  * @param id2label Mapping of SDFA ids to SPSA labels.
  * @param state2row Mapping of SDFA states to matrix rows.
  * @param transitionMatrix The transition matrix.
  * @param iso The isomoprhism mapping SDFA minterms to symbols.
  * @param startStates The set of start states.
  * @param maxOrder The maximum order of the underlying SPSA.
  * @param id The SDFA's unique id.
  * @param partitionAttribute The partition attribute of the SDFA's pattern.
  */
class SPSAInterface(
                     val sdfa: SDFA,
                     val label2id: Map[(Int, SymbolWord), Int],
                     val id2label: Map[Int, (Int, SymbolWord)],
                     val state2row: Map[Int, Int],
                     val transitionMatrix: DenseMatrix[Double],
                     val iso: Isomorphism,
                     val startStates: Set[(Int, SymbolWord)],
                     val maxOrder: Int,
                     val id: Int,
                     val partitionAttribute: String
                   ) extends FSMInterface with Serializable {

  /**
    * @return The final states.
    */
  override def getFinals: Set[Int] = sdfa.finals

  /**
    * @return The FSM's ID.
    */
  override def getId: Int = id

  /**
    * @return The non-final states.
    */
  override def getNonFinals: Set[Int] = sdfa.states.keySet &~ sdfa.finals

  /**
    * @return The states of the FSM.
    */
  override def getStates: Set[Int] = sdfa.states.keySet

  /**
    * Can have multiple start states, just give one for now.
    *
    * @return The start state.
    */
  override def getStartId: Int = sdfa.start //label2id(spsa.getStartStates.head)

  override def getRuntimeWindow: Int = 0

  override def getWindowType: WindowType = COUNT

  /**
    * Given a (current) state of the FSM and a new input event, find the next state.
    *
    * @param currentState The FSM's current state.
    * @param event The new input event.
    * @param buffer The buffer holding the current context/suffix. Not used here.
    * @param conf The current configuration (state + valuation) of the automaton. Not used here.
    * @return The next state that the FSM moves to.
    */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent,
                             buffer: CyclicBuffer,
                             conf: Configuration
                           ): Set[Configuration] = Set(sdfa.getDelta(currentState, event).head)

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
                        ): Boolean = sdfa.connected(from, to)

  /**
    * Temporary, currently serializes the SDFA and writes it to a file.
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
    * Returns true if the SPSA can start with a given word.
    * A SPSA can have multiple start states and each start state can have its own label. In order to find the start
    * state from which the SPSA can begin, we need to make sure that there exists one start start state whose label
    * is a suffix of the given word. This is necessary because, during the construction of the PSA from which we build
    * the SPSA, we do not include any transient states. Is is an unnecessary overhead, since in a streaming setting,
    * the PSA quickly moves to a recurrent class. We can then choose to simply wait for a couple of symbols/events and
    * then start the SPSA, as soon as we find a start state that is proper (i.e., is a suffix of the stream).
    *
    * @param word The given word.
    * @return True if the SPSA can start with the given word.
    */
  def canStartWith(word: SymbolWord): Option[Int] = {
    val starting = startStates.find(s => s._2.isSuffixOf(word))
    starting match {
      case Some(x) => Option(label2id(x))
      case None => None
    }
  }

  /**
    * Constructs the Markov chain of the SPSA.
    *
    * @return The automaton's Markov chain.
    */
  def getMarkovChain: MarkovChain = {
    val mc = MarkovChainFactory.buildMC(transitionMatrix, state2row, getFinalsNo)
    mc
  }

  override def toString: String = "temporary"

}
