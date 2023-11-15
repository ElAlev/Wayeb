package fsm

import breeze.linalg.DenseMatrix
import com.typesafe.scalalogging.LazyLogging
import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sra.Configuration
import fsm.symbolic.sra.dsra.DSRAStreaming
import model.markov.TransitionProbs
import model.vmm.mapper.SymbolExtractorFromDSRA
import model.vmm.pst.{CyclicBuffer, PredictionSuffixTree}
import model.vmm.{Symbol, SymbolWord}
import model.waitingTime.WtDistribution
import stream.GenericEvent

object SPSTmInterface {
  /**
   * Constructor for the SPSTm interface.
   *
   * @param pst The PST.
   * @param dsra Ths DSRA.
   * @param symEx The symbol extractor.
   * @param maxOrder The maximum order (maximum depth) of the PST.
   * @param id The SPSTm's unique id.
   * @param partitionAttribute The partition attribute of the DSRA's pattern.
   * @return The SPSTm interace.
   */
  def apply(
             pst: PredictionSuffixTree,
             dsra: DSRAStreaming,
             symEx: SymbolExtractorFromDSRA,
             maxOrder: Int,
             id: Int,
             partitionAttribute: String
           ): SPSTmInterface = new SPSTmInterface(pst, dsra, symEx, maxOrder, id, partitionAttribute)
}

/**
 *
 * Wrapper for symbolic prediction suffix trees with memory (SPSTm).
 * Extends fsm.FSMInterface.
 * A SPSTm has two main components: a DSRA and a prediction suffix tree (PST) learned from the symbols produced by the
 * transitions of the DSRA (each DSRA transition corresponds to a symbol).
 * A SPSTm can be viewed as a virtual automaton. It is virtual in the sense that we do not explicitly create its
 * transitions. Each virtual state of the SPSTm is derived by combining a node of the PST with a state of the DSRA.
 * In order to determine the behavior of the SPSTm whenever a new input event arrives, we maintain a buffer that always
 * holds the last maxOrder events of the stream. For every new event, we need to determine the next state of the DSRA
 * and the next node of the PST. For the DSRA state, we simply feed the DSRA with the new event. For the PST node,
 * we insert the new event to the buffer and then traverse the tree using the contents of the buffer. Note also that
 * we do not need a transition matrix, since we compute the waiting-time distributions by projecting the possible
 * future paths that the SPSTm may follow.
 *
 * @param pst The PST.
 * @param dsra The DSRA.
 * @param symEx The symbol extractor, i.e., the DSRA where each transition has been assigned a symbol.
 * @param maxOrder The maximum order (maximum depth) of the PST.
 * @param id The SPSTm's unique id.
 * @param partitionAttribute The partition attribute of the DSRA's pattern.
 */
class SPSTmInterface(
                      val pst: PredictionSuffixTree,
                      val dsra: DSRAStreaming,
                      val symEx: SymbolExtractorFromDSRA,
                      val maxOrder: Int,
                      val id: Int,
                      val partitionAttribute: String
                    ) extends FSMInterface with Serializable with LazyLogging {
  // first get all the labels from the nodes of the PST
  private val labels = pst.getAllLabels

  // then get all the DSRA states
  private val dsraStates = dsra.hyperStates.toSet

  // now combine PST labels and SDFA states to get all the virtual states
  private val labelsXStates = labels.flatMap(l => dsraStates.map(s => (l, s))).toList

  // give ids to these virtual states
  private val states = (1 to labelsXStates.size).toList

  // create a mapping from PST labels/DSRA states to SPSTm virtual state ids
  private val toStates = labelsXStates.zip(states).toMap

  // create a mapping from SPSTm virtual state ids to PST labels/DSRA states
  private val fromStates = states.zip(labelsXStates).toMap

  // isolate the final virtual states
  private val finals = toStates.filter(x => dsra.isFinal(x._1._2)).values.toSet

  //isolate the start virtual states
  private val start = toStates.filter(x => dsra.isStart(x._1._2)).values.toSet

  // create a DSRA wrapper in order to have access to some of its functionality
  private val dsrai = DSRAInterface(dsra, id, partitionAttribute)

  /**
   * @return The final states.
   */
  override def getFinals: Set[Int] = finals

  /**
   * @return The states of the FSM.
   */
  override def getStates: Set[Int] = states.toSet

  /**
   * Can have multiple start states, just give one for now.
   *
   * @return The start state.
   */
  override def getStartId: Int = start.head

  /**
   * @return The FSM's ID.
   */
  override def getId: Int = id

  override def getRuntimeWindow: Int = 0

  override def getWindowType: WindowType = COUNT

  /**
   * Actually does nothing here, just creates a dummy matrix.
   *
   * @param probs The set of conditional probabilities.
   * @return A transition matrix corresponding to the given conditional probabilities.
   */
  override def buildTransitionMatrix(probs: TransitionProbs): DenseMatrix[Double] = new DenseMatrix[Double](0, 0)

  /**
   * Temporary, now just serializes the DSRA and writes it to a file.
   *
   * @param fn The path to the file.
   */
  override def write2File(fn: String): Unit = dsra.write2File(fn)

  /**
   * @return The partition attribute
   */
  override def getPartitionAttribute: String = partitionAttribute

  /**
   * Checks whether there is a transition from a given DSRA state to another DSRA state.
   *
   * @param from The candidate source state.
   * @param to The candidate target state.
   * @return Return true if there is indeed a transition.
   */
  override def connected(
                          from: Int,
                          to: Int
                        ): Boolean = {
    val dsraFrom = getDSRAStateFor(from)
    val dsraTo = getDSRAStateFor(to)
    dsra.connected(dsraFrom, dsraTo)
  }

  /**
   * Finds the next virtual state, given the current virtual state, a new input event and the contents of the buffer.
   *
   * @param currentState The FSM's current state.
   * @param event The new input event.
   * @param buffer The buffer holding the current context/suffix.
   * @param conf The current configuration (state + valuation) of the automaton.
   * @return The next state that the FSM moves to.
   */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent,
                             buffer: CyclicBuffer,
                             conf: Configuration
                           ): Set[Configuration] = {
    // first find the current DSRA state from the current virtual state
    val dsraCurrentState = getDSRAStateFor(currentState)
    val dsraConf = Configuration(dsraCurrentState, conf.valuation, conf.index, conf.symbol)
    // now find the next DSRA configuration
    val nextConf = dsra.yieldsSuccessorConfig(dsraConf, event).head
    // we now move on to find the next PST node label
    // first get the context, i.e., the contents of the buffer
    val buffered = buffer.pop
    // traverse the tree with the context o find the node
    val pstState = pst.getNodeUntilLeafNonBlocking(buffered)
    // the node's label and the DSRA's state id comprise our next state
    val labelXState = (pstState.label, nextConf.stateId)
    // convert this next state to a virtual state id
    val nextState = toStates(labelXState)
    Set(Configuration(nextState, nextConf.valuation, nextConf.index, nextConf.symbol))
  }

  /**
   * For a given virtual state, returns the DSRA corresponding state.
   *
   * @param state The given virtual state.
   * @return The corresponding virtual state.
   */
  private def getDSRAStateFor(state: Int): Int = fromStates(state)._2

  /**
   * Approximate, incremental computation of waiting-time distributions. See paper for details. Significantly more
   * efficient than the exhaustive computation. Future sequences are constructed incrementally. A sequence is not
   * expanded after encountering a final state. A sequence is also dropped if its probability is below the cutoff
   * threshold (this optimization is what makes the computation approximate).
   *
   * @param horizon The horizon.
   * @param wtCutoffThreshold The cutoff threshold.
   * @param distance Distances range for the states that will actually produce forecasts.
   * @return The approximate waiting-time distributions.
   */
  def computeWtDistsOpt(
                         horizon: Int,
                         wtCutoffThreshold: Double,
                         distance: (Double, Double)
                       ): Map[Int, WtDistribution] = {
    val symbolsList = symEx.getSymbols
    if (distance._1 != -1 & distance._1 < 1.0) estimateRemainingPercentage
    val retainedStates =
      if (remainingPercentage.nonEmpty)
        states.filter(state => remainingPercentage(state) >= distance._1 & remainingPercentage(state) <= distance._2)
      else states
    logger.debug("Estimating distributions for " + retainedStates.length + " states")
    val wts = retainedStates.sorted.map(s => {
      val statesAt0 = List((fromStates(s), 1.0))
      val currentWt = Map.empty[Int, Double]
      (s, computeWtDistForHorizonOpt(statesAt0, 1, horizon, symbolsList, currentWt, wtCutoffThreshold))
    })
    wts.toMap
  }

  /**
   * Recursive, incremental computation of a waiting-time distribution. We start from the current state (given as
   * PST label/DSRA id) and its probability (set to 1.0 for the first step). We then perform a one step expansion.
   * We find all next states that are final, sum their probabilities and set the corresponding entry in the
   * distribution. We then keep only those expansions that lead to non-final states and those with a probability
   * above the threshold. These retained expansions are given are input for the next iteration of the recursion.
   *
   * @param statesAtTminus1 The sequences and their probabilities that are alive until now and will be checked for
   *                        further expansion.
   * @param t The index of the distribution that we are going to check.
   * @param horizon The horizon.
   * @param symbols All the symbols that may be encountered.
   * @param currentWt The wt distribution up to the current index t-1.
   * @param wtCutoffThreshold The cutoff threshold.
   * @return The waiting-time distribution.
   */
  @scala.annotation.tailrec
  private def computeWtDistForHorizonOpt(
                                          statesAtTminus1: List[((List[Symbol], Int), Double)],
                                          t: Int,
                                          horizon: Int,
                                          symbols: List[Symbol],
                                          currentWt: Map[Int, Double],
                                          wtCutoffThreshold: Double
                                        ): WtDistribution = {
    if (t > horizon) WtDistribution(currentWt)
    else {
      // perform the one step expansion
      val statesAtT = statesAtTminus1.flatMap(x => oneStepAheadProbsOpt(x._1._1, x._1._2, x._2, symbols))
      // partition the sequences to those that end in a final state and those than end to a non-final
      val statesPartitions = statesAtT.partition(x => dsrai.isFinal(x._1._2))
      // sum the probabilities of the sequences that end in a final state
      val finalProb = statesPartitions._1.map(x => x._2).sum
      // add new entry to the distribution
      val newWt = currentWt + (t -> finalProb)
      val nonFinals = statesPartitions._2
      // from the other sequences, filter out those that have a probability below the cutoff threshold
      val nonFinalsFiltered = nonFinals.filter(x => x._2 > wtCutoffThreshold)
      // new iteration
      computeWtDistForHorizonOpt(nonFinalsFiltered, t + 1, horizon, symbols, newWt, wtCutoffThreshold)
    }
  }

  private def oneStepAheadProbsOpt(
                                    label: List[Symbol],
                                    currentState: Int,
                                    prob: Double,
                                    symbols: List[Symbol]
                                  ): List[((List[Symbol], Int), Double)] = {
    val nextDsraStates = symbols.map(s => symEx.dsra.getNextStateWithSymbol(currentState, s))
    val nextPstLabels = symbols.map(s => pst.getNodeUntilLeafNonBlocking(s :: label).label)
    val probs = symbols.map(s => pst.getConditionalProbFor(s, label) * prob)
    val nextStates = nextPstLabels.zip(nextDsraStates)
    val statesWithProbs = nextStates.zip(probs)
    statesWithProbs
  }

  def canStartWith(word: SymbolWord): Option[Int] = {
    val node = pst.getNodeUntilLeafNonBlocking(word.word)
    if (node.isLeaf) {
      val state = toStates((node.label, dsra.start))
      Option(state)
    } else None
  }

}
