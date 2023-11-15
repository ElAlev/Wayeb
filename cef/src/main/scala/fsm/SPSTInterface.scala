package fsm

import breeze.linalg.DenseMatrix
import com.typesafe.scalalogging.LazyLogging
import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.symbolic.sra.Configuration
import model.markov.TransitionProbs
import model.vmm.mapper.{Isomorphism, SymbolMapper}
import stream.GenericEvent
import utils.SetUtils
import model.vmm.{Symbol, SymbolWord}
import model.vmm.pst.{CyclicBuffer, PredictionSuffixTree}
import model.waitingTime.WtDistribution
import ui.ConfigUtils

object SPSTInterface {

  /**
    * Constructor for SPST interace with default partition attribute (none).
    *
    * @param pst The PST.
    * @param sdfa The SDFA.
    * @param iso The isomorphism mapping SDFA minterms to symbols.
    * @param maxOrder The maximum order (maximum depth) of the PST.
    * @param id The SPST's unique id.
    * @return The SPST interace.
    */
  def apply(
             pst: PredictionSuffixTree,
             sdfa: SDFA,
             iso: Isomorphism,
             maxOrder: Int,
             id: Int
           ): SPSTInterface = new SPSTInterface(pst, sdfa, iso, maxOrder, id, ConfigUtils.singlePartitionVal)

  /**
    * Constructor for SPST interace.
    *
    * @param pst The PST.
    * @param sdfa The SDFA.
    * @param iso The isomorphism mapping SDFA minterms to symbols.
    * @param maxOrder The maximum order (maximum depth) of the PST.
    * @param id The SPST's unique id.
    * @param partitionAttribute The partition attribute of the SDFA's pattern.
    * @return The SPST interace.
    */
  def apply(
             pst: PredictionSuffixTree,
             sdfa: SDFA,
             iso: Isomorphism,
             maxOrder: Int,
             id: Int,
             partitionAttribute: String
           ): SPSTInterface = new SPSTInterface(pst, sdfa, iso, maxOrder, id, partitionAttribute)

  /**
   * Constructor for SPST interace where the isomorphism is given as a symbol mapper.
   *
   * @param pst                The PST.
   * @param sdfa               The SDFA.
   * @param sm                 The symbol mapper (should be an isomorphism).
   * @param maxOrder           The maximum order (maximum depth) of the PST.
   * @param id                 The SPST's unique id.
   * @param partitionAttribute The partition attribute of the SDFA's pattern.
   * @return The SPST interace.
   */
  def apply(
             pst: PredictionSuffixTree,
             sdfa: SDFA,
             sm: SymbolMapper,
             maxOrder: Int,
             id: Int,
             partitionAttribute: String
           ): SPSTInterface = SPSTInterface(pst, sdfa, sm.asInstanceOf[Isomorphism], maxOrder, id, partitionAttribute)

}

/**
  * Wrapper for symbolic prediction suffix trees (SPST).
  * Extends fsm.FSMInterface.
  * A SPST has two main components: a SDFA and a prediction suffix tree (PST) learned with the minterms of the SDFA.
  * A SPST can be viewed as a virtual automaton. It is virtual in the sense that we do not explicitly create its
  * transitions. Each virtual state of the SPST is derived by combining a node of the PST with a state of the SDFA.
  * In order to determine the behavior of the SPST whenever a new input event arrives, we maintain a buffer that always
  * holds the last maxOrder events of the stream. For every new event, we need to determine the next state of the SDFA
  * and the next node of the PST. For the SDFA state, we simply feed the SDFA with the new event. For the PST node,
  * we insert the new event to the buffer and then traverse the tree using the contents of the buffer. Note also that
  * we do not need a transition matrix, since we compute the waiting-time distributions by projecting the possible
  * future paths that the SPST may follow.
  *
  * @param pst The PST.
  * @param sdfa The SDFA.
  * @param iso The isomorphism mapping SDFA minterms to symbols.
  * @param maxOrder The maximum order (maximum depth) of the PST.
  * @param id The SPST's unique id.
  * @param partitionAttribute The partition attribute of the SDFA's pattern.
  */
class SPSTInterface(
                     val pst: PredictionSuffixTree,
                     val sdfa: SDFA,
                     val iso: Isomorphism,
                     val maxOrder: Int,
                     val id: Int,
                     val partitionAttribute: String
                   ) extends FSMInterface with Serializable with LazyLogging {
  // first get all the labels from the nodes of the PST
  private val labels = pst.getAllLabels

  // then get all the SDFA states
  private val fsmStates: Set[Int] = sdfa.states.keySet

  // now combine PST labels and SDFA states to get all the virtual states
  private val labelsXStates = labels.flatMap(l => fsmStates.map(s => (l, s))).toList

  // give ids to these virtual states
  private val states = (1 to labelsXStates.size).toList

  // create a mapping from PST labels/SDFA states to SPST virtual state ids
  private val toStates = labelsXStates.zip(states).toMap

  // create a mapping from SPST virtual state ids to PST labels/SDFA states
  private val fromStates: Map[Int, (List[Symbol], Int)] = states.zip(labelsXStates).toMap

  // isolate the final virtual states
  private val finals = toStates.filter(x => sdfa.isFinal(x._1._2)).values.toSet

  //isolate the start virtual states
  private val start = toStates.filter(x => sdfa.isStart(x._1._2)).values.toSet

  // create a SDFA wrapper in order to have access to some of its functionality
  private val sdfai = SDFAInterface(sdfa, id, partitionAttribute)

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
    * Temporary, now just serializes the SDFA and writes it to a file.
    *
    * @param fn The path to the file.
    */
  override def write2File(fn: String): Unit = sdfa.write2File(fn)

  /**
    * @return The partition attribute
    */
  override def getPartitionAttribute: String = partitionAttribute

  /**
    * Checks whether there is a transition from a given SDFA state to another sdfa state.
    *
    * @param from The candidate source state.
    * @param to The candidate target state.
    * @return Return true if there is indeed a transition.
    */
  override def connected(
                          from: Int,
                          to: Int
                        ): Boolean = {
    val sdfaFrom = getSdfaStateFor(from)
    val sdfaTo = getSdfaStateFor(to)
    sdfa.connected(sdfaFrom, sdfaTo)
  }

  /**
    * Finds the next virtual state, given the current virtual state, a new input event and the contents of the buffer.
    *
    * @param currentState The FSM's current state.
    * @param event The new input event.
    * @param buffer The buffer holding the current context/suffix.
    * @param conf The current configuration (state + valuation) of the automaton. Not used here.
    * @return The next state that the FSM moves to.
    */
  override def getNextState(
                             currentState: Int,
                             event: GenericEvent,
                             buffer: CyclicBuffer,
                             conf: Configuration
                           ): Set[Configuration] = {
    // first find the current SDFA state from the current virtual state
    val sdfaCurrentState = getSdfaStateFor(currentState)
    // now find the next SDFA state
    val sdfaNextConf = sdfa.getDelta(sdfaCurrentState, event).head
    // we now move on to find the next PST node label
    // first get the context, i.e., the contents of the buffer
    val buffered = buffer.pop
    // traverse the tree with the context o find the node
    val pstState = pst.getNodeUntilLeafNonBlocking(buffered)
    // the node's label and the SDFA's state id comprise our next state
    val labelXState = (pstState.label, sdfaNextConf.stateId)
    // convert this next state to a virtual state id
    val nextState = toStates(labelXState)
    Set(Configuration(nextState, sdfaNextConf.output))
  }

  /**
    * @return The total number of virtual states.
    */
  def getSize: Int = states.length

  /**
   * @return The total number of labels, i.e., tree leaves.
   */
  def getLabelsSize: Int = labels.size

  /**
    * For a given virtual state, returns the SDFA corresponding state.
    *
    * @param state The given virtual state.
    * @return The corresponding virtual state.
    */
  private def getSdfaStateFor(state: Int): Int = fromStates(state)._2

  /**
    * We override this method and use the SDFA to find the remaining percentages. For every virtual state, we use the
    * SDFA's corresponding state to find its remaining percentage.
    *
    * @return The remaining percentage of every state.
    */
  override def estimateRemainingPercentage: Map[Int, Double] = {
    logger.debug("Estimating remaining percentages...")
    sdfai.estimateRemainingPercentage
    shortestPathDistances = states.map(s => (s, sdfai.shortestPathDistances(getSdfaStateFor(s)))).toMap
    remainingPercentage = states.map(s => (s, sdfai.remainingPercentage(getSdfaStateFor(s)))).toMap
    remainingPercentage
  }

  /**
    * Computes the pseudo waiting-time distributions. A pseudo wt distribution has the same form as an actual wt
    * distribution, but is not in fact a wt distribution. It is estimated in a simplistic manner. For a given horizon,
    * we find all the possible future symbol sequences (i.e., all permutations of symbols of length equal to the
    * horizon). We then find the most probable future symbol sequence. For this sequence, we perform "forward
    * recognition" and get a sequence of future SDFA states. We then find the first visit to a final state (if such a
    * visit exists) and we set the probability of reaching a final state at this index to 1.0 and 0.0 for all other
    * indexes. Pseudo wt distributions are used to test whether the feasibility of the idea of finding the most
    * probable future sequence and then performing forward recognition on top of this sequence. We retain the form of a
    * wt distribution so that we can reuse other components that assume a wt distribution (e.g. predictors that build
    * forecast intervals).
    *
    * @param horizon The horizon.
    * @return The pseudo waiting-time distribution.
    */
  def computePseudoWtDists(horizon: Int): Map[Int, WtDistribution] = {
    val symbols = iso.getSymbols.toSet
    logger.debug("Estimating permutations")
    val permutations = SetUtils.permutationsAlt[Symbol](symbols, horizon)(horizon)
    logger.debug("Done with permutations")
    logger.debug("Estimating pseudo wt-distributions for " + states.length + " states")
    states.sorted.map(state => (state, computePseudoWtDist(state, horizon, permutations))).toMap
  }

  /**
    * Computes the pseudo waiting-time distribution for a given state out of a set of future symbol sequences.
    *
    * @param state The state for which we estimate the pseudo wt distribution.
    * @param horizon The horizon.
    * @param futureSequences A set of future symbol sequences out of which we will select the most probable and build
    *                        the distribution.
    * @return The pseudo waiting-time distribution for the state.
    */
  private def computePseudoWtDist(
                                   state: Int,
                                   horizon: Int,
                                   futureSequences: Set[List[Symbol]]
                                 ): WtDistribution = {
    logger.debug("Estimating pseudo wt-distribution for state " + state)
    val (label, sdfaState) = fromStates(state)
    val futureSequencesList = futureSequences.toList
    // find the probabilities of all sequences
    val seqWithProbs = futureSequencesList.map(p => (p, pst.getConditionalProbFor(p, label)))
    // sort by probability
    val sortedByProb = seqWithProbs.sortWith((x, y) => x._2 > y._2)
    // keep the first/highest
    val mostProbableFutureSequence = sortedByProb.head
    // convert the symbol sequence to a sequence of minterms
    val futureSentences = mostProbableFutureSequence._1.reverse.map(s => iso.getMinTermForSymbol(s))
    // now get the sequence of states from the sequence of minterms
    val projectedStates = sdfai.getNextStateWithSentences(sdfaState, futureSentences)
    // find the index of the first visit to a final state
    val visit2final = findFirstVisitToFinal(projectedStates, 1)
    val wtEntries = (1 to projectedStates.length).map(i => {
      // For the index of the first visit, set the probability to 1.0
      if (i == visit2final) (i, 1.0) else (i, 0.0)
    }).toMap
    WtDistribution(wtEntries)
  }

  @scala.annotation.tailrec
  private def findFirstVisitToFinal(
                                     states: List[Int],
                                     i: Int
                                   ): Int = {
    states match {
      case Nil => -1
      case head :: tail => if (sdfai.isFinal(head)) i else findFirstVisitToFinal(tail, i + 1)
    }
  }

  /**
    * Computes the true waiting-time distributions by finding all future symbol sequences and their probabilities.
    * Expensive, since it exhaustively finds all future sequences. Used for testing against approximate estimations.
    *
    * @param horizon The horizon.
    * @return The true waiting-time distributions.
    */
  def computeWtDistsExhaustive(horizon: Int): Map[Int, WtDistribution] = {
    val symbols = iso.getSymbols.toSet
    logger.debug("Estimating permutations")
    val permutations = SetUtils.permutationsAlt[Symbol](symbols, horizon)(horizon)
    logger.debug("Done with permutations")
    logger.debug("Estimating distributions for " + states.length + " states")
    states.sorted.map(state => (state, computeWtDistExhaustive(state, horizon, permutations))).toMap
  }

  /**
    * Computes the true waiting-time distribution for a given state out of a set of future symbol sequences.
    *
    * @param state The state for which we estimate the pseudo wt distribution.
    * @param horizon The horizon.
    * @param futureSequences A set of future symbol sequences from which we will estimate the distribution.
    * @return The true waiting-time distribution.
    */
  private def computeWtDistExhaustive(
                                       state: Int,
                                       horizon: Int,
                                       futureSequences: Set[List[Symbol]]
                                     ): WtDistribution = {
    logger.debug("Estimating distribution for state " + state)
    val (label, sdfaState) = fromStates(state)
    val futureSequencesList = futureSequences.toList
    // find the probabilities of all sequences
    val seqWithProbs = futureSequencesList.map(p => (p, pst.getConditionalProbFor(p, label)))
    // convert symbol sequences to sequences of minterms.
    val futureSentencesList = futureSequencesList.map(seq => seq.reverse.map(sym => iso.getMinTermForSymbol(sym)))
    // convert minterm sequences to states sequences
    val futureStatesList = futureSentencesList.map(s => sdfai.getNextStateWithSentences(sdfaState, s))
    // join state sequences with their probabilities
    val futureStatesWithProbs = futureStatesList.zip(seqWithProbs.map(x => x._2))
    // for each state sequence find the index where it visits a final state for the first time
    val futureStatesWithProbsIndices = futureStatesWithProbs.map(x => (firstVisitToFinal(x._1), x._2))
    // now group all sequences by their first visit index
    val futureStatesGroupedByFirstVisit = futureStatesWithProbsIndices.filter(i => i._1 > 0).groupBy(x => x._1)
    // .map(identity) used to make class serializable (otherwise mapValues prevents serialization)
    // and now sum all probabilities withing groups
    val futureStatesGroupedByFirstVisitProbs = futureStatesGroupedByFirstVisit.mapValues(x => x.map(y => y._2).sum).map(identity)

    // some indices may have zero probability, so we add them manually
    val allFutureIndices = (1 to horizon).toSet
    val indicesWithNonZeroProb = futureStatesGroupedByFirstVisitProbs.keySet
    val indicesWithZeroProb = allFutureIndices &~ indicesWithNonZeroProb
    val wtZeroEntries = indicesWithZeroProb.map(i => (i, 0.0)).toMap

    val wt = wtZeroEntries ++ futureStatesGroupedByFirstVisitProbs
    WtDistribution(wt)
  }

  private def firstVisitToFinal(futureStates: List[Int]): Int = {
    val visitsToFinal = futureStates.map(s => sdfa.isFinal(s))
    val firstVisitIndex = visitsToFinal.indexWhere(_ == true) + 1
    //firstVisitIndex == t
    firstVisitIndex
  }

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
    val symbolsList = iso.getSymbols
    if (distance._1 != -1 & distance._1 < 1.0) estimateRemainingPercentage
    val retainedStates =
      if (remainingPercentage.nonEmpty)
        states.filter(state => remainingPercentage(state) >= distance._1 & remainingPercentage(state) <= distance._2)
      else states
    logger.debug("Estimating distributions for " + retainedStates.length + " states")
    //val dfai = DFAProvider(DFASourceFromSDFA(sdfa,iso)).provide().head
    val wts = retainedStates.sorted.map(s => {
      val statesAt0 = List((fromStates(s), 1.0))
      val currentWt = Map.empty[Int, Double]
      //logger.debug("Estimating distribution for state " + s)
      //(s,computeWtDistForHorizonOpt(dfai,statesAt0,1,horizon,symbolsList,currentWt))
      (s, computeWtDistForHorizonOpt(statesAt0, 1, horizon, symbolsList, currentWt, wtCutoffThreshold))
    })
    wts.toMap
  }

  /**
    * Same as fsm.SPSTInterface#computeWtDistsOpt(int, double, scala.Tuple2), but produces distributions for all states.
    *
    * @param horizon The horizon.
    * @param wtCutoffThreshold The cutoff threshold.
    * @return The approximate waiting-time distributions.
    */
  def computeWtDistsOpt(
                         horizon: Int,
                         wtCutoffThreshold: Double
                       ): Map[Int, WtDistribution] = {
    computeWtDistsOpt(horizon, wtCutoffThreshold, (0.0, 1.0))
  }

  def computeWtDistsOpt(horizon: Int): Map[Int, WtDistribution] = computeWtDistsOpt(horizon, ConfigUtils.wtCutoffThreshold, (0.0, 1.0))

  /**
    * Recursive, incremental computation of a waiting-time distribution. We start from the current state (given as
    * PST label/SDFA id) and its probability (set to 1.0 for the first step). We then perform a one step expansion.
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
                                          //dfai: DFAInterface,
                                          statesAtTminus1: List[((List[Symbol], Int), Double)],
                                          t: Int,
                                          horizon: Int,
                                          symbols: List[Symbol],
                                          currentWt: Map[Int, Double],
                                          wtCutoffThreshold: Double
                                        ): WtDistribution = {
    if (t > horizon) WtDistribution(currentWt)
    else {
      //val statesAtT = statesAtTminus1.flatMap(x => oneStepAheadProbsOpt(dfai,x._1._1, x._1._2, x._2, symbols))
      // perform the one step expansion
      val statesAtT = statesAtTminus1.flatMap(x => oneStepAheadProbsOpt(x._1._1, x._1._2, x._2, symbols))
      // partition the sequences to those that end in a final state and those than end to a non-final
      val statesPartitions = statesAtT.partition(x => sdfai.isFinal(x._1._2))
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
                                    //dfai: DFAInterface,
                                    label: List[Symbol],
                                    sdfaState: Int,
                                    prob: Double,
                                    symbols: List[Symbol]
                                  ): List[((List[Symbol], Int), Double)] = {
    val sentences = symbols.map(s => iso.getMinTermForSymbol(s))
    val nextSdfaStates = sentences.map(s => sdfai.getNextStateWithSentence(sdfaState, s))
    //val nextSdfaStates = sentences.map(s => dfai.dfa.delta(sdfaState,s.toString))
    val nextPstLabels = symbols.map(s => pst.getNodeUntilLeafNonBlocking(s :: label).label)
    val probs = symbols.map(s => pst.getConditionalProbFor(s, label) * prob)
    val nextStates = nextPstLabels.zip(nextSdfaStates)
    val statesWithProbs = nextStates.zip(probs)
    statesWithProbs
  }

  /**
    * Incremental computation of waiting-time distributions.
    * Essentially the same as fsm.SPSTInterface#computeWtDistsOpt(int, double), without the optimization of pruning
    * non-final expansions whose probability is below a threshold. It is incremental. Does not expand paths that have
    * a final state.
    *
    * @param horizon The horizon.
    * @return The waiting-time distribution.
    */
  def computeWtDists(horizon: Int): Map[Int, WtDistribution] = {
    val symbolsList = iso.getSymbols
    logger.debug("Estimating distributions for " + states.length + " states")
    val wts = states.sorted.map(s => {
      val statesAt0 = List((s, 1.0))
      val currentWt = Map.empty[Int, Double]
      //logger.debug("Estimating distribution for state " + s)
      (s, computeWtDistForHorizon(statesAt0, 1, horizon, symbolsList, currentWt))
    })
    wts.toMap
  }

  @scala.annotation.tailrec
  private def computeWtDistForHorizon(
                                       statesAtTminus1: List[(Int, Double)],
                                       t: Int,
                                       horizon: Int,
                                       symbols: List[Symbol],
                                       currentWt: Map[Int, Double]
                                     ): WtDistribution = {
    if (t > horizon) WtDistribution(currentWt)
    else {
      val statesAtT = statesAtTminus1.flatMap(x => oneStepAheadProbs(x._1, x._2, symbols))
      val statesPartitions = statesAtT.partition(x => isFinal(x._1))
      val finalProb = statesPartitions._1.map(x => x._2).sum
      val newWt = currentWt + (t -> finalProb)
      computeWtDistForHorizon(statesPartitions._2, t + 1, horizon, symbols, newWt)
    }
  }

  private def oneStepAheadProbs(
                                 state: Int,
                                 prob: Double,
                                 symbols: List[Symbol]
                               ): List[(Int, Double)] = {
    val (label, sdfaState) = fromStates(state)
    val sentences = symbols.map(s => iso.getMinTermForSymbol(s))
    val nextSdfaStates = sentences.map(s => sdfai.getNextStateWithSentence(sdfaState, s))
    val nextPstLabels = symbols.map(s => pst.getNodeUntilLeafNonBlocking(s :: label).label)
    val probs = symbols.map(s => pst.getConditionalProbFor(s, label) * prob)
    val nextStates = nextPstLabels.zip(nextSdfaStates).map(x => toStates((x._1, x._2)))
    val statesWithProbs = nextStates.zip(probs)
    statesWithProbs
  }

  def canStartWith(word: SymbolWord): Option[Int] = {
    val node = pst.getNodeUntilLeafNonBlocking(word.word)
    if (node.isLeaf) {
      val state = toStates((node.label, sdfa.start))
      Option(state)
    } else None
  }

}
