package model.vmm.pst.psa

import breeze.stats.distributions.Uniform
import com.typesafe.scalalogging.LazyLogging
import utils.SetUtils
import model.vmm.pst.{PredictionSuffixTree, SymbolDistribution}
import model.vmm.{Symbol, SymbolWord}
import ui.ConfigUtils

/**
  * Utils for working with probabilistic suffix automata.
  */
object PSAUtils extends LazyLogging {
  /**
    * Converts a prediction suffix tree to a probabilistic suffix automaton.
    * Assumes that the PST is already PSA compatible.
    * If not, then model.vmm.pst.PredictionSuffixTree#makePSACompatible() should first be called on the PST.
    *
    * @param pst The prediction suffix tree.
    * @return The equivalent probabilistic suffix automaton.
    */
  def buildPSA(pst: PredictionSuffixTree): ProbSuffixAutomaton = {
    if (pst.getSize == 1) {
      // No meaningful suffixes found, so the PSA is a single state with all transitions coming back to it
      val singleState = PSAState(SymbolWord(pst.label))
      val dist = pst.getDistribution.dist
      val transitions = dist.map(x => (x._1, PSATransition(singleState, x._1, x._2)))
      singleState.setTransitionsToNextStates(transitions)
      val states = Map[SymbolWord, PSAState](SymbolWord(pst.label) -> singleState)
      ProbSuffixAutomaton(states)
    }
    else {
      // else use the PST leaves as PSA states
      val leaves = pst.getLeaves
      val symbols = pst.getSymbols
      val states = leaves.map(l => (SymbolWord(l.label), PSAState(SymbolWord(l.label)))).toMap
      states.foreach(s => addNextStates(s._2, symbols, states, pst))
      ProbSuffixAutomaton(states)
    }
  }

  /**
    * Creates the outgoing transitions from a given PSA state for the given symbols.
    *
    * @param state The given PSA state.
    * @param symbols The given symbols.
    * @param states All the PSA states.
    * @param pst The PST.
    */
  private def addNextStates(
                             state: PSAState,
                             symbols: Set[Symbol],
                             states: Map[SymbolWord, PSAState],
                             pst: PredictionSuffixTree
                           ): Unit = {
    val nextStates = symbols.map(s => (s, findNextState(state, s, states, pst))).toMap
    state.setTransitionsToNextStates(nextStates)
  }

  /**
    * Finds the next PSA state from a given PSA state with a given symbol and creates the relevant transition.
    *
    * @param state The given PSA state.
    * @param symbol The transition's symbol.
    * @param states All the PSA states.
    * @param pst The PST.
    * @return The PSA transition.
    */
  private def findNextState(
                             state: PSAState,
                             symbol: Symbol,
                             states: Map[SymbolWord, PSAState],
                             pst: PredictionSuffixTree
                           ): PSATransition = {
    val newSuffix = SymbolWord(symbol :: state.label.word)
    // Slow. better use Suffix tree to find next state.
    /*val nextState = states.find(s => isSuffixOf(s._1,newSuffix))
    nextState match {
      case Some(p) => p._2
      case None => throw new Error("Could not find next state")
    }*/
    val newSuffixNode = pst.getNodeUntilLeafBlocking(newSuffix.word)
    if (!newSuffixNode.isLeaf) {
      println("OOPS")
      logger.warn(newSuffix.word.toString() + " not a leaf in " + pst.toString)
    }
    require(newSuffixNode.isLeaf)
    val nextStateLabel = SymbolWord(newSuffixNode.label)
    require(states.contains(nextStateLabel))
    val oldSuffixNode = pst.getNodeUntilLeafBlocking(state.label.word)
    val prob = oldSuffixNode.getProbFor(symbol)
    PSATransition(states(nextStateLabel), symbol, prob)
  }

  /**
    * From a set of symbols, creates all suffix sets of maximum order maxOrder that are maxORder-complete and proper.
    *
    * @param symbols All symbols.
    * @param maxOrder The maximum order.
    * @return A set with all complete and proper suffix sets.
    */
  def createAllCompleteProperSuffixSets(
                                         symbols: Set[Symbol],
                                         maxOrder: Int
                                       ): Set[Set[SymbolWord]] = {
    val allSuffixes = SetUtils.permutationsAlt(symbols, maxOrder).values.flatten.toSet[List[Symbol]].map(p => SymbolWord(p))
    val allSuffixSets = SetUtils.power(allSuffixes).filter(ss => ss.nonEmpty)
    val properComplete = allSuffixSets.filter(ss => isCompleteProper(ss, symbols, maxOrder))
    properComplete
  }

  /**
    * Determines whether all suffixes of a given set are transition-full, maxOrder-complete and proper.
    *
    * @param suffixes All suffixes to be checked.
    * @param symbols All symbols.
    * @param maxOrder The maximum order.
    * @return True is suffix set is transition-full, maxOrder-complete and proper.
    */
  def isCompleteProperFull(
                            suffixes: Set[SymbolWord],
                            symbols: Set[Symbol],
                            maxOrder: Int
                          ): Boolean = {
    isCompleteProper(suffixes, symbols, maxOrder) & isFull(suffixes, symbols)
  }

  /**
    * Determines whether a set of suffixes is complete and proper.
    * A suffix set is maxOrder-complete if for all words of length maxOrder there exists at least one suffix that is a
    * suffix of the word. In other words, if we encounter a word of length maxOrder, we can always find a suffix for it.
    * A suffix set is proper if for every suffix there does not exist another one that is a suffix of it. In other
    * words, for every possible word of length maxOrder we can unambiguously determine which suffix to use, i.e., there
    * is only one suffix of the word.
    *
    * @param suffixes The set of suffixes to be checked.
    * @param symbols All symbols.
    * @param maxOrder The maximum order.
    * @return True if the suffix set is complete and proper.
    */
  def isCompleteProper(
                        suffixes: Set[SymbolWord],
                        symbols: Set[Symbol],
                        maxOrder: Int
                      ): Boolean = {
    val allSuffixes = SetUtils.permutationsAlt(symbols, maxOrder)(maxOrder).map(p => SymbolWord(p))
    val complete = allSuffixes.forall(fullSuffix => suffixes.exists(suffix => suffix.isSuffixOf(fullSuffix)))
    //val complete = isComplete(suffixes,symbols,maxOrder)
    val proper = suffixes.forall(suffix => {
      val restSuffixes = suffixes - suffix
      !restSuffixes.exists(r => r.isSuffixOf(suffix))
    })
    //val full = suffixes.forall(suffix => transitionFull(suffix,suffixes,symbols))
    complete & proper //& full
  }

  /**
    * Determines whether all suffixes of a given set are transition-full.
    *
    * @param suffixes All suffixes to be checked.
    * @param symbols All symbols.
    * @return True if all suffixes are transition-full.
    */
  def isFull(
              suffixes: Set[SymbolWord],
              symbols: Set[Symbol]
            ): Boolean = {
    suffixes.forall(suffix => transitionFull(suffix, suffixes, symbols))
  }

  /**
    * Determines whether a given suffix is transition-full.
    * A suffix is transition-full with respect to a given set of symbols and a given set of suffixes if,
    * for every symbol, if we append it to the suffix, there is always another suffix that is a suffix of the expanded
    * suffix. In other words, is we assume a given context, then, for every symbol we know that there will be a suffix
    * for the new context.
    *
    * @param suffix The suffix to check.
    * @param suffixes The given set of suffixes.
    * @param symbols The given set of symbols.
    * @return True if the suffix is transition-full.
    */
  private def transitionFull(
                              suffix: SymbolWord,
                              suffixes: Set[SymbolWord],
                              symbols: Set[Symbol]
                            ): Boolean = {
    //symbols.forall(symbol => suffixes.exists(otherSuffix => otherSuffix.isSuffixOf(suffix.appendHead(symbol))))
    var full = true
    for (symbol <- symbols) {
      var symbolFull = false
      for (otherSuffix <- suffixes)
        if (otherSuffix.isSuffixOf(suffix.appendHead(symbol))) symbolFull = true
      if (!symbolFull)
        full = false
    }
    full
  }

  /**
    * Creates a random suffix set that is complete, proper and full. We start with single symbols and then randomly try
    * to expand each one to all words of length 2. Those that are not chosen to be expanded are retained. We repeat the
    * same process with the suffixes of length 2. Some of them will be expanded to length 3. The rest are retained. We
    * repeat and stop when we reach suffixes of length maxOrder at which point we stop expanding. We thus end up with a
    * set that has some suffixes of length 1, some of length 2, etc, with those of higher length being more rare.
    *
    * @param symbols All symbols to be used.
    * @param maxOrder The maximum order.
    * @param expansionProb The expansion probability with which we determine whether to expand suffixes.
    * @return A complete, proper and full suffix set.
    */
  def createCompleteProperFullSuffixSet(
                                         symbols: Set[Symbol],
                                         maxOrder: Int,
                                         expansionProb: Double
                                       ): Set[SymbolWord] = {
    var suffixes = createCompleteProperSuffixSet(symbols, maxOrder, expansionProb)
    while (!isFull(suffixes, symbols)) suffixes = createCompleteProperSuffixSet(symbols, maxOrder, expansionProb)
    suffixes
  }

  /**
    * Creates a random suffix set that is complete and proper, but maybe not full.
    *
    * @param symbols All symbols to be used.
    * @param maxOrder The maximum order.
    * @param expansionProb The expansion probability with which we determine whether to expand suffixes.
    * @return A complete and proper suffix set.
    */
  private def createCompleteProperSuffixSet(
                                             symbols: Set[Symbol],
                                             maxOrder: Int,
                                             expansionProb: Double
                                           ): Set[SymbolWord] = {
    require(symbols.nonEmpty)
    require(maxOrder > 0)
    require(expansionProb > 0.0 & expansionProb < 1.0)
    val uni = new Uniform(0, 1)
    val front = symbols.map(symbol => SymbolWord(List(symbol)))
    createCompleteProperSuffixSetAux(symbols, maxOrder, expansionProb, front, uni)
  }

  /**
    * Auxiliary recursive function to create a complete and proper suffix set.
    *
    * @param symbols All symbols to be used.
    * @param maxOrder The maximum order.
    * @param expansionProb The expansion probability with which we determine whether to expand suffixes.
    * @param front The current set of suffixes that can still be expanded.
    * @param uni The uniform distributio from which we draw samples in oder to determine whether to expand or not.
    * @return A complete and proper suffix set.
    */
  private def createCompleteProperSuffixSetAux(
                                                symbols: Set[Symbol],
                                                maxOrder: Int,
                                                expansionProb: Double,
                                                front: Set[SymbolWord],
                                                uni: Uniform
                                              ): Set[SymbolWord] = {
    var suffixes = Set.empty[SymbolWord]
    var thisFront: Set[SymbolWord] = front
    while (thisFront.nonEmpty) {
      val candidate = thisFront.head
      if (candidate.length == maxOrder) suffixes += candidate
      else {
        val sample = uni.sample()
        if (sample < expansionProb) {
          val newFront = symbols.map(symbol => candidate.appendTail(symbol))
          suffixes ++= createCompleteProperSuffixSetAux(symbols, maxOrder, expansionProb, newFront, uni)
        }
        else suffixes += candidate
      }
      thisFront -= candidate
    }
    suffixes
  }

  /**
    * Determines whether the labels of a PSA form a suffix-free and full set.
    *
    * @param psa The PSA.
    * @return True if its labels are a suffix-free and full set.
    */
  def isSuffixFreeFull(psa: ProbSuffixAutomaton): Boolean = isSuffixFree(psa) & isSuffixFull(psa)

  /**
    * Determines whether the labels of a PSA form a suffix-free set.
    *
    * @param psa The PSA.
    * @return True if its labels are a suffix-free set.
    */
  def isSuffixFree(psa: ProbSuffixAutomaton): Boolean = isSuffixFree(psa.getLabels)

  /**
    * Determines whether a set of strings is suffix-free.
    * A set of strings S is called a suffix free set if ∀s ∈ S, Suffix ∗ (s) ∩ S = {s},
    * where the set of all suffixes of s is denoted by Suffix ∗ (s) = {si . . . sl | 1 ≤ i ≤ l} ∪ {e}.
    *
    * @param strings The set of strings to be checked.
    * @return True if it is suffix-free.
    */
  private def isSuffixFree(strings: Set[SymbolWord]): Boolean = {
    //labels.forall( label => (labels & wordSuffixSet(label,Set.empty)) == label )
    var free = true
    for (label <- strings) {
      val wordSuffixes = wordSuffixSet(label, Set.empty)
      val intersection = strings & wordSuffixes
      if (intersection != Set(label))
        free = false
    }
    free
  }

  /**
    * Auxiliary recursive function that creates all suffixes of a string.
    * The set of all suffixes of a string s is denoted by Suffix ∗ (s) = {si . . . sl | 1 ≤ i ≤ l} ∪ {e}.
    *
    * @param word The string whose suffixes we want to find.
    * @param suffixes The current suffixes.
    * @return All suffixes.
    */
  @scala.annotation.tailrec
  private def wordSuffixSet(
                             word: SymbolWord,
                             suffixes: Set[SymbolWord]
                           ): Set[SymbolWord] = {
    if (word.isEmpty) suffixes
    else {
      val newSuffix = SymbolWord(word.word.take(word.length - 1))
      wordSuffixSet(newSuffix, suffixes + word)
    }

  }

  /**
    * Determines whether the set of labels of a PSA are a full set.
    *
    * @param psa The PSA.
    * @return True if its labels are a full set.
    */
  def isSuffixFull(psa: ProbSuffixAutomaton): Boolean = {
    var full = true
    val states = psa.states
    val labels = psa.getLabels
    val symbols = psa.getSymbols
    for (state <- states) {
      for (symbol <- symbols) {
        if (state._2.getProbFor(symbol) > 0) {
          val suffixSigma = state._1.appendHead(symbol)
          if (!labels.exists(label => label.isSuffixOf(suffixSigma))) full = false
        }
      }
    }
    full
  }

  /**
    * Creates a random PSA of max order maxOrder.
    *
    * @param symbols All symbols of the PSA.
    * @param maxOrder The PSA's maximum order.
    * @param expansionProb The expansion probability. The higher it is, the more probable it is for the PSA to have
    *                      high order states.
    * @return The new PSA.
    */
  def createPSA(
                 symbols: Set[Symbol],
                 maxOrder: Int,
                 expansionProb: Double
               ): ProbSuffixAutomaton = {
    require(symbols.nonEmpty)
    require(maxOrder > 0)
    require(expansionProb > 0.0 & expansionProb < 1.0)
    var psa = createPSAAux1(symbols, maxOrder, expansionProb)
    // Might happen that generated PSA is not suffix free. Keep trying until you get it right!
    while (!PSAUtils.isSuffixFreeFull(psa)) psa = createPSAAux1(symbols, maxOrder, expansionProb)
    psa
  }

  /**
    * Auxiliary function to create a PSA.
    * First create an initial label and then use this as a first state and keep expanding until maxOrder.
    *
    * @param symbols All symbols of the PSA.
    * @param maxOrder The PSA's maximum order.
    * @param expansionProb the expansion probability.
    * @return The PSA.
    */
  private def createPSAAux1(
                             symbols: Set[Symbol],
                             maxOrder: Int,
                             expansionProb: Double
                           ): ProbSuffixAutomaton = {
    val firstLabel = generateSymbolWord(symbols, maxOrder, expansionProb)
    val firstState = PSAState(firstLabel)
    val existingStates: Map[SymbolWord, PSAState] = Map(firstLabel -> firstState)
    val states = createPSAAux2(symbols, maxOrder, expansionProb, existingStates, Set(firstState))
    val psa = ProbSuffixAutomaton(states)
    psa
  }

  /**
    * Auxiliary recursive function to create a PSA.
    *
    * @param symbols All symbols of the PSA.
    * @param maxOrder The PSA's maximum order.
    * @param expansionProb the expansion probability.
    * @param existingStates The states created thus far.
    * @param frontier The states for which we need to add outgoing transitions.
    * @return The states of the PSA with their labels.
    */
  @scala.annotation.tailrec
  private def createPSAAux2(
                             symbols: Set[Symbol],
                             maxOrder: Int,
                             expansionProb: Double,
                             existingStates: Map[SymbolWord, PSAState],
                             frontier: Set[PSAState]
                           ): Map[SymbolWord, PSAState] = {
    if (frontier.isEmpty) existingStates
    else {
      var newFront = Set.empty[PSAState]
      var newExistingStates = existingStates
      for (state <- frontier) {
        // we need to check each state of the frontier (unchecked states)
        var transitions: Map[Symbol, PSATransition] = Map.empty[Symbol, PSATransition]
        val nextSymbolDistribution = generateSymbolDistribution(symbols)
        for (symbol <- symbols) {
          // we need to see where we will move with each symbol
          val suffixSigma = state.label.appendHead(symbol)
          val nextExistingStates = newExistingStates.keySet.filter(label => label.isSuffixOf(suffixSigma))
          require(nextExistingStates.size <= 1)
          if (nextExistingStates.nonEmpty) {
            // if the next state already exists (i.e., there exists a state with a label that is a suffix of the current
            // context appended with the symbol), then create relevant transitions
            val nextExistingState = nextExistingStates.head
            val newTransition = PSATransition(newExistingStates(nextExistingState), symbol, nextSymbolDistribution.getProbFor(symbol))
            transitions += (symbol -> newTransition)
          }
          else {
            // if the next state does not exist, we need to create it
            // we can choose which label to create
            // in the simplest case we could choose the single symbol itself as a label
            // but we can keep expanding it until maxOrder
            val nextState = createNewState(newExistingStates, suffixSigma, maxOrder, expansionProb)
            val newTransition = PSATransition(nextState, symbol, nextSymbolDistribution.getProbFor(symbol))
            transitions += (symbol -> newTransition)
            newExistingStates += (nextState.label -> nextState)
            /*if (!isSuffixFree(newExistingStates.keySet)) println("PROBLEMO")
            require(isSuffixFree(newExistingStates.keySet))*/
            newFront += nextState
          }
        }
        state.setTransitionsToNextStates(transitions)
      }
      createPSAAux2(symbols, maxOrder, expansionProb, newExistingStates, newFront)
    }
  }

  /**
    * Finds and creates a new state for a given context + new symbol. We start from a label of length 1 and try to
    * expand it according to a probability (we increase the current index).
    *
    * @param existingStates All existing states.
    * @param suffixSigma context + new symbol.
    * @param maxOrder The maximum length of the new state's label.
    * @param expansionProb The expansion probability.
    * @return The new PSA state.
    */
  private def createNewState(
                              existingStates: Map[SymbolWord, PSAState],
                              suffixSigma: SymbolWord,
                              maxOrder: Int,
                              expansionProb: Double
                            ): PSAState = {
    val uni = Uniform(0, 1)
    createNewStateAux(existingStates, 1, suffixSigma, maxOrder, expansionProb, uni)
  }

  /**
    * Auxiliary recursive function to create a new PSA state for a given context + new symbol.
    *
    * @param existingStates All existing states.
    * @param currentIndex The length of the current suffix.
    * @param suffix context + new symbol.
    * @param maxOrder The maximum length of the new state's label.
    * @param expansionProb The expansion probability.
    * @param uni The uniform distribution from which to draw samples in order to decide expansion.
    * @return The new PSA state.
    */
  @scala.annotation.tailrec
  private def createNewStateAux(
                                 existingStates: Map[SymbolWord, PSAState],
                                 currentIndex: Int,
                                 suffix: SymbolWord,
                                 maxOrder: Int,
                                 expansionProb: Double,
                                 uni: Uniform
                               ): PSAState = {
    val currentSuffix = SymbolWord(suffix.word.take(currentIndex))
    // if we have reach the maximum order or the length of the original label, then do not expand any further
    if (currentSuffix.length == maxOrder | currentSuffix.length == suffix.length) PSAState(currentSuffix)
    else {
      // if there already exists a state whose label is a suffix extension of the current suffix, keep expanding
      // there's no point in creating a state with the current suffix as label because this suffix would lead us to
      // both states
      if (existingStates.keySet.exists(label => currentSuffix.isSuffixOf(label)))
        createNewStateAux(existingStates, currentIndex + 1, suffix, maxOrder, expansionProb, uni)
      else {
        val expand = uni.sample()
        if (expand < expansionProb)
          createNewStateAux(existingStates, currentIndex + 1, suffix, maxOrder, expansionProb, uni)
        else PSAState(currentSuffix)
      }
    }
  }

  /**
    * Randomly generates a string of maximum length maxOrder. Starts with a string of length 1 and then randomly decides
    * to expand it to length 2. Repeats the same process. Process stops either because the random decision does not
    * allow another expansion or we have reached maxOrder.
    *
    * @param symbols All symbols to be used.
    * @param maxOrder The maximum order.
    * @param expansionProb The expansion probability. The higher it is, the more probable that the string will be of
    *                      lengthier.
    * @return A string of length at most maxOrder.
    */
  private def generateSymbolWord(
                                  symbols: Set[Symbol],
                                  maxOrder: Int,
                                  expansionProb: Double
                                ): SymbolWord = {
    val uni = new Uniform(0, 1)
    generateSymbolWordAux(symbols, maxOrder, expansionProb, SymbolWord(List(sampleSymbol(symbols))), uni)
  }

  /**
    * Auxiliary recursive function to randomly generate a string of maximum length maxOrder.
    *
    * @param symbols All symbols to be used.
    * @param maxOrder The maximum order.
    * @param expansionProb The expansion probability. The higher it is, the more probable that the string will be of
    *                      lengthier.
    * @param word The string generated thus far.
    * @param uni The distribution from which to draw samples in order to decide the expansion.
    * @return A string of length at most maxOrder.
    */
  @scala.annotation.tailrec
  private def generateSymbolWordAux(
                                     symbols: Set[Symbol],
                                     maxOrder: Int,
                                     expansionProb: Double,
                                     word: SymbolWord,
                                     uni: Uniform
                                   ): SymbolWord = {
    if (word.length == maxOrder) word
    else {
      val s = uni.sample()
      if (s < expansionProb) {
        val symbol = sampleSymbol(symbols)
        generateSymbolWordAux(symbols, maxOrder, expansionProb, word.appendTail(symbol), uni)
      } else word
    }
  }

  /**
    * Randomly, uniformly picks a symbol from a given set.
    *
    * @param symbols The given set of symbols.
    * @return One symbol randomly chosen.
    */
  private def sampleSymbol(symbols: Set[Symbol]): Symbol = {
    val uni = Uniform(0, symbols.size - 0.001)
    val index = uni.sample().toInt
    val a = symbols.toArray
    a(index)
  }

  /**
    * Generates a random next symbol distribution.
    *
    * @param symbols The possible next symbols.
    * @return The next symbol distribution.
    */
  private def generateSymbolDistribution(symbols: Set[Symbol]): SymbolDistribution = {
    val probs = generateProbs(symbols.size)
    val probsMap = symbols.toList.zip(probs).toMap
    SymbolDistribution(probsMap)
  }

  /**
    * Generates a list of probabilities. The sum should be close to 1.0.
    *
    * @param symbolsNo The number of points to generate.
    * @return The list of probabilities.
    */
  private def generateProbs(symbolsNo: Int): List[Double] = {
    val probs = generateProbsAux(symbolsNo, List.empty[Double])
    val probsSum = probs.sum
    val returnProbs =
      if (probsSum == 1.0) probs
      else if (probsSum < 1.0) {
        val newProbs = addRemainder(probs)
        newProbs
      }
      else {
        val newProbs = subtractRedundancy(probs)
        newProbs
      }
    require(returnProbs.sum > 1.0 - ConfigUtils.consistencyTolerance)
    require(returnProbs.sum < 1.0 + ConfigUtils.consistencyTolerance)
    returnProbs
  }

  /**
    * In a list of probabilities that sum to less than 1.0, we try to re-normalize them so that they add to 1.0.
    * We estimate the remainder and then scan the probabilities to see where we can add the remainder (is we try to add
    * it to a probability and the new probability is greater than 1.0, we move on to the next).
    *
    * @param probs The list of probabilities to be re-normalized.
    * @return The re-normalized list.
    */
  private def addRemainder(probs: List[Double]): List[Double] = {
    val probsSum = probs.sum
    val remainder = 1.0 - probsSum
    addRemainderAux(probs, List.empty, remainder)
  }

  /**
    * Auxiliary recursive function to re-normalize a list of probabilities that sum to less than 1.0.
    *
    * @param remainingProbs The probabilities that have not been checked yet.
    * @param scannedProbs Those that have been checked.
    * @param remainder The remainder that we need to add somewhere.
    * @return The re-normalized list.
    */
  @scala.annotation.tailrec
  private def addRemainderAux(
                               remainingProbs: List[Double],
                               scannedProbs: List[Double],
                               remainder: Double
                             ): List[Double] = {
    if (remainingProbs.isEmpty) scannedProbs
    else {
      val candidateProb = remainingProbs.head + remainder
      if (candidateProb > 1.0) addRemainderAux(remainingProbs.tail, scannedProbs ::: List(remainingProbs.head), remainder)
      else scannedProbs ::: List(candidateProb) ::: remainingProbs.tail

    }
  }

  /**
    * In a list of probabilities that sum to more than 1.0, we try to re-normalize them so that they add to 1.0.
    * We estimate the redundancy and then scan the probabilities to see where we can subtract the redundancy (if we try
    * to subtract it from a probability and the new probability is less than 0.0, we move on to the next).
    *
    * @param probs The list of probabilities to be re-normalized.
    * @return The re-normalized list.
    */
  private def subtractRedundancy(probs: List[Double]): List[Double] = {
    val probsSum = probs.sum
    val remainder = probsSum - 1.0
    subtractRedundancyAux(probs, List.empty, remainder)
  }

  /**
    * Auxiliary recursive function to re-normalize a list of probabilities that sum to more than 1.0.
    *
    * @param remainingProbs The probabilities that have not been checked yet.
    * @param scannedProbs Those that have been checked.
    * @param redundancy The redundancy that we need to subtract from somewhere.
    * @return The re-normalized list.
    */
  @scala.annotation.tailrec
  private def subtractRedundancyAux(
                                     remainingProbs: List[Double],
                                     scannedProbs: List[Double],
                                     redundancy: Double
                                   ): List[Double] = {
    if (remainingProbs.isEmpty) scannedProbs
    else {
      val candidateProb = remainingProbs.head - redundancy
      if (candidateProb < 0.0) subtractRedundancyAux(remainingProbs.tail, scannedProbs ::: List(remainingProbs.head), redundancy)
      else scannedProbs ::: List(candidateProb) ::: remainingProbs.tail
    }
  }

  /**
    * Auxiliary recursive function to generate a list of probabilities. The sum should be close to 1.0.
    *
    * @param symbolsNo The number of points to generate.
    * @param probs The list generated thus far.
    * @return The list of probabilities (might not sum exactly to 1.0).
    */
  @scala.annotation.tailrec
  private def generateProbsAux(
                                symbolsNo: Int,
                                probs: List[Double]
                              ): List[Double] = {
    if (probs.size == symbolsNo) probs
    else if (probs.size == symbolsNo - 1)
    // if only one point remains to be added, just add what remains after subtracting from 1.0
      generateProbsAux(symbolsNo, (1 - probs.sum) :: probs)
    else {
      // randomly choose a probability from what still remains after subtracting current sum from 1.0
      val uni = Uniform(0, 1 - probs.sum)
      val newProb = uni.sample()
      generateProbsAux(symbolsNo, newProb :: probs)
    }
  }

  /**
    * Creates a PSA with only two symbols (0,1). We start with two given states/labels and add whatever else is
    * required.
    *
    * @param label1 The first label.
    * @param prob1For0 The transition probability from the first state with 0.
    * @param label2 The second label.
    * @param prob2For0 The transition probability from the second state with 0.
    * @param targetProb The transition probability from every other state with 0.
    * @return The PSA.
    */
  def createPSA01(
                   label1: SymbolWord,
                   prob1For0: Double,
                   label2: SymbolWord,
                   prob2For0: Double,
                   targetProb: Double
                 ): ProbSuffixAutomaton = {
    require(label1.length == label2.length)
    val maxOrder = label1.length
    val state1 = PSAState(label1)
    val state2 = PSAState(label2)

    val existingStates: Map[SymbolWord, PSAState] = Map(label1 -> state1, label2 -> state2)

    val newStates1 = findTransitionsStates01(state1, existingStates, maxOrder, prob1For0)
    val existingStates1 = existingStates ++ newStates1
    val newStates2 = findTransitionsStates01(state2, existingStates1, maxOrder, prob2For0)
    val existingStates2 = existingStates1 ++ newStates2

    val initFront = newStates1.values.toSet ++ newStates2.values.toSet
    val allStates = createPSA01Aux(existingStates2, initFront, maxOrder, targetProb)
    val psa = ProbSuffixAutomaton(allStates)
    require(PSAUtils.isSuffixFreeFull(psa))
    psa

  }

  /**
    * Finds and creates all transitions and target states from a given state.
    *
    * @param state The given state.
    * @param existingStates The already existing states.
    * @param maxOrder The PSA's maximum order.
    * @param targetProb The transition probability with 0.
    * @return All new target states.
    */
  private def findTransitionsStates01(
                                       state: PSAState,
                                       existingStates: Map[SymbolWord, PSAState],
                                       maxOrder: Int,
                                       targetProb: Double
                                     ): Map[SymbolWord, PSAState] = {
    var transitions: Map[Symbol, PSATransition] = Map.empty[Symbol, PSATransition]
    val symbol0 = Symbol(0)
    val symbol1 = Symbol(1)
    var newStates: Map[SymbolWord, PSAState] = Map.empty

    var suffixSigma = state.label.appendHead(symbol0)
    val nextStatesFor0 = existingStates.filter(s => s._1.isSuffixOf(suffixSigma))
    val nextStateFor0 =
      if (nextStatesFor0.nonEmpty) nextStatesFor0.head._2
      else {
        val newState = createNewState(existingStates, suffixSigma, maxOrder, 0.0)
        newStates += (newState.label -> newState)
        newState
      }
    transitions += (symbol0 -> PSATransition(nextStateFor0, symbol0, targetProb))

    suffixSigma = state.label.appendHead(symbol1)
    val nextStatesFor1 = (existingStates ++ newStates).filter(s => s._1.isSuffixOf(suffixSigma))
    val nextStateFor1 =
      if (nextStatesFor1.nonEmpty) nextStatesFor1.head._2
      else {
        val newState = createNewState(existingStates ++ newStates, suffixSigma, maxOrder, 0.0)
        newStates += (newState.label -> newState)
        newState
      }
    transitions += (symbol1 -> PSATransition(nextStateFor1, symbol1, 1 - targetProb))

    state.setTransitionsToNextStates(transitions)

    newStates
  }

  /**
    * Auxiliary recursive function to create all states for a PSA.
    *
    * @param existingStates The already existing states (initially the two first states).
    * @param frontier The current set of PSA states for which we need to add outgoing transitions.
    * @param maxOrder The PSA's maximum order.
    * @param targetProb The transition probability with 0.
    * @return All PSA states.
    */
  @scala.annotation.tailrec
  private def createPSA01Aux(
                              existingStates: Map[SymbolWord, PSAState],
                              frontier: Set[PSAState],
                              maxOrder: Int,
                              targetProb: Double
                            ): Map[SymbolWord, PSAState] = {
    if (frontier.isEmpty) existingStates
    else {
      var newFront = Set.empty[PSAState]
      var newExistingStates = existingStates
      for (state <- frontier) {
        val newStates = findTransitionsStates01(state, newExistingStates, maxOrder, targetProb)
        newExistingStates = newExistingStates ++ newStates
        newFront = newFront ++ newStates.values.toSet
      }
      createPSA01Aux(newExistingStates, newFront, maxOrder, targetProb)
    }
  }

  /*@scala.annotation.tailrec
  private def isSuffixOf(
                          word1: SymbolWord,
                          word2: SymbolWord
                        ): Boolean = {
    if (word1.length > word2.length) false
    else {
      if (word1.isEmpty) true
      else {
        val head1 = word1.head
        val head2 = word2.head
        if (head1 != head2) false
        else isSuffixOf(word1.tail, word2.tail)
      }
    }
  }

  private def isComplete(
                          suffixes: Set[SymbolWord],
                          symbols: Set[Symbol],
                          maxOrder: Int
                        ): Boolean = {
    val allSuffixes = SetUtils.permutationsAlt(symbols, maxOrder)(maxOrder).map(p => SymbolWord(p))
    var complete = true
    for (fullSuffix <- allSuffixes) {
      var existsSuffix = false
      for (suffix <- suffixes) {
        if (suffix.isSuffixOf(fullSuffix)) existsSuffix = true
      }
      if (!existsSuffix) {
        complete = false
        println("No suffix for " + fullSuffix)
      }
    }
    complete
  }*/
}
