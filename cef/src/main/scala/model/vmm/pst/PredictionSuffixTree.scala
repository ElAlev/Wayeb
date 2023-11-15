package model.vmm.pst

import com.typesafe.scalalogging.LazyLogging
import utils.{MathUtils, StringUtils}
import model.vmm.Symbol
import model.vmm.mapper.Isomorphism

object PredictionSuffixTree {
  /**
    * Constructor for Prediction Suffix Trees.
    *
    * @param label The node's label as a list of symbols.
    * @param symbolDistribution The node's next symbol distribution.
    * @param c The node's children.
    * @return A new prediction suffix tree.
    */
  def apply(
             label: List[Symbol],
             symbolDistribution: SymbolDistribution,
             c: Map[Symbol, PredictionSuffixTree]
           ): PredictionSuffixTree = new PredictionSuffixTree(label, symbolDistribution, c)

  /**
    * Constructor for Prediction Suffix Trees without any children.
    *
    * @param label The node's label as a list of symbols.
    * @param symbolDistribution The node's next symbol distribution.
    * @return A new prediction suffix tree.
    */
  def apply(
             label: List[Symbol],
             symbolDistribution: SymbolDistribution
           ): PredictionSuffixTree = new PredictionSuffixTree(label, symbolDistribution, Map.empty[Symbol, PredictionSuffixTree])

  /**
    * Constructor for an empty Prediction Suffix Trees.
    *
    * @return An empty prediction suffix tree.
    */
  def apply(): PredictionSuffixTree = new PredictionSuffixTree(
    List.empty[Symbol],
    SymbolDistribution(),
    Map.empty[Symbol, PredictionSuffixTree]
  )
}

/**
  * Class for prediction suffix trees.
  * See
  * @article{DBLP:journals/ml/RonST96,
  *                                   author    = {Dana Ron and
  *                                   Yoram Singer and
  *                                   Naftali Tishby},
  *                                   title     = {The Power of Amnesia: Learning Probabilistic Automata with Variable
  *                                   Memory Length},
  *                                   journal   = {Mach. Learn.},
  *                                   volume    = {25},
  *                                   number    = {2-3},
  *                                   pages     = {117--149},
  *                                   year      = {1996}
  *                                   }
  *
  * Let Σ be an alphabet. A PST T over Σ is a tree whose edges are labeled by symbols σ ∈ Σ and each internal node has
  * exactly one edge for every σ ∈ Σ (hence, the degree is | Σ |). Each node is labeled by a pair (s, γs), where s is
  * the string associated with the walk starting from that node and ending in the root, and γs : Σ → [0, 1] is the next
  * symbol probability function related with s. For every string s labeling a node, P σ∈Σ γs(σ) = 1.
  *
  *
  * @param label The node's label as a list of symbols.
  * @param symbolDistribution The node's next symbol distribution.
  * @param c The node's children.
  */
class PredictionSuffixTree(
                            val label: List[Symbol],
                            symbolDistribution: SymbolDistribution,
                            c: Map[Symbol, PredictionSuffixTree]
                          ) extends LazyLogging with Serializable {

  private var dist = symbolDistribution
  private var children = c

  /**
    * Updates the tree with a new suffix. Traverses the tree and creates nodes that do not exist.
    *
    * @param suffix The new suffix.
    * @param cst The CST from which next symbol distributions can be derived.
    * @param withSmoothing If true, the next symbol distributions will be smoothed.
    * @param gammaMin Smallest probability used for smoothing.
    * @param allSymbols Set will all tree symbols. Also required for smoothing.
    * @return The last (deepest) node created/reached.
    */
  def updateWithNewSuffix(
                           suffix: List[Symbol],
                           cst: CounterSuffixTree,
                           withSmoothing: Boolean,
                           gammaMin: Double,
                           allSymbols: Set[Symbol]
                         ): PredictionSuffixTree = {
    suffix match {
      case head :: tail => {
        if (children.contains(head)) {
          val child = children(head)
          child.updateWithNewSuffix(tail, cst, withSmoothing, gammaMin, allSymbols)
        } else {
          val newChild = addNewChild(head, cst, withSmoothing, gammaMin, allSymbols)
          newChild.updateWithNewSuffix(tail, cst, withSmoothing, gammaMin, allSymbols)
        }
      }
      case Nil => this
    }
  }

  /**
    * Updates the tree with a new suffix. Traverses the tree and creates nodes that do not exist. Performs no
    * distribution smoothing.
    *
    * @param suffix The new suffix.
    * @param cst The CST from which next symbol distributions can be derived.
    * @return The last (deepest) node created/reached.
    */
  def updateWithNewSuffix(
                           suffix: List[Symbol],
                           cst: CounterSuffixTree
                         ): PredictionSuffixTree =
    updateWithNewSuffix(suffix, cst, withSmoothing = false, 0.0, Set.empty)

  /**
    * Adds a new child with the given symbol to this node.
    *
    * @param symbol The given symbol.
    * @param cst The CST from which to derive next symbol distributions.
    * @param withSmoothing If true, the next symbol distributions will be smoothed.
    * @param gammaMin Smallest probability used for smoothing.
    * @param allSymbols Set will all tree symbols. Also required for smoothing.
    * @return The new child.
    */
  private def addNewChild(
                           symbol: Symbol,
                           cst: CounterSuffixTree,
                           withSmoothing: Boolean,
                           gammaMin: Double,
                           allSymbols: Set[Symbol]
                         ): PredictionSuffixTree = {
    require(!children.contains(symbol))
    // new label is created by appending at the end (oldest) of the current label the symbol
    val newLabel = label ::: List(symbol)
    val newSymbolDistribution =
      if (withSmoothing) cst.getSymbolDistributionFor(newLabel).smoothSer(gammaMin, allSymbols)
      else cst.getSymbolDistributionFor(newLabel)
    val newChild = PredictionSuffixTree(newLabel, newSymbolDistribution)
    children += (symbol -> newChild)
    newChild
  }

  /**
    * Adds a new child with the given symbol to this node. No smoothing.
    *
    * @param symbol The given symbol.
    * @param cst The CST from which to derive next symbol distributions.
    * @return The new child.
    */
  private def addNewChild(
                           symbol: Symbol,
                           cst: CounterSuffixTree
                         ): PredictionSuffixTree =
    addNewChild(symbol, cst, withSmoothing = false, 0.0, Set.empty)


  /**
    * Adds missing children of internal nodes. If an internal node does not have a child for a symbol, we add this
    * child. Recursive function working level by level.
    *
    * @param symbols All symbols to be found in the tree.
    * @param root The root of the tree. Always passed to recursive calls.
    * @param cst The CST from which to derive next symbol distributions.
    */
  def addMissingChildren(
                          symbols: Set[Symbol],
                          root: PredictionSuffixTree,
                          cst: CounterSuffixTree
                        ): Unit = {
    val childrenSymbols = children.keys.toSet
    if (childrenSymbols.nonEmpty) {
      // find missing symbols
      val symbolsToAdd = symbols &~ childrenSymbols
      for (newSymbol <- symbolsToAdd) {
        // add them to this node
        addMissingChild(newSymbol, root, cst)
      }
    }
    // move on to next level, but follow only children that already existed (ingore the ones just created)
    children.filter(c => childrenSymbols.contains(c._1)).foreach(x => x._2.addMissingChildren(symbols, root, cst))
  }

  /**
    * Adds a new missing child to the node.
    *
    * @param symbol The new child's symbol.
    * @param root The root of the node's tree.
    * @param cst The CST from which to derive next symbol distributions.
    * @param withSmoothing If true, the next symbol distributions will be smoothed.
    * @param gammaMin Smallest probability used for smoothing.
    * @param allSymbols Set will all tree symbols. Also required for smoothing.
    * @return The new child.
    */
  private def addMissingChild(
                               symbol: Symbol,
                               root: PredictionSuffixTree,
                               cst: CounterSuffixTree,
                               withSmoothing: Boolean,
                               gammaMin: Double,
                               allSymbols: Set[Symbol]
                             ): PredictionSuffixTree = {
    require(!children.contains(symbol))
    val newLabel = label ::: List(symbol)
    // in order to find which symbol distribution to use, we need to find the longest suffix of this new node's label
    // that already exists in the tree and get this suffix's distribution
    val longestSuffix = root.findLongestSuffix(newLabel)
    // TODO: why calculate distribution again, maybe just retrieve it directly from the root
    val newSymbolDistribution =
      if (withSmoothing) cst.getSymbolDistributionFor(longestSuffix).smoothSer(gammaMin, allSymbols)
      else cst.getSymbolDistributionFor(longestSuffix)
    val newChild = PredictionSuffixTree(newLabel, newSymbolDistribution)
    children += (symbol -> newChild)
    newChild
  }

  /**
    * Adds a new missing child to the node. No smoothing.
    *
    * @param symbol The new child's symbol.
    * @param root The root of the node's tree.
    * @param cst The CST from which to derive next symbol distributions.
    * @return The new child.
    */
  private def addMissingChild(
                               symbol: Symbol,
                               root: PredictionSuffixTree,
                               cst: CounterSuffixTree
                             ): PredictionSuffixTree =
    addMissingChild(symbol, root, cst, withSmoothing = false, 0.0, Set.empty)

  /**
    * Finds the longest suffix of the given word that already exists in the tree
    *
    * @param word The given word as a list of symbols (head is the most recent symbol).
    * @return The longest suffix as a list of symbols.
    */
  @scala.annotation.tailrec
  private def findLongestSuffix(word: List[Symbol]): List[Symbol] = {
    val subtree = getSubtreeFor(word)
    subtree match {
      case Some(_) => word
      case None => {
        val newSuffixToCheck = word.take(word.size - 1)
        findLongestSuffix(newSuffixToCheck)
      }
    }
  }

  /**
    * Retrieves the node corresponding to a given word. Head of the word should be the last/most recent symbol.
    * If the word does not exist, None is returned.
    *
    * @param word The given word.
    * @return The node of the word or None if word does not exist.
    */
  private def getSubtreeFor(word: List[Symbol]): Option[PredictionSuffixTree] = getSubtreeForAux(word, this)

  /**
    * Auxiliary recursive function to find the node for a given word.
    *
    * @param word The current word.
    * @param subtree The current node.
    * @return The node of the word or None if word does not exist.
    */
  @scala.annotation.tailrec
  private def getSubtreeForAux(
                                word: List[Symbol],
                                subtree: PredictionSuffixTree
                              ): Option[PredictionSuffixTree] = {
    word match {
      case head :: tail => {
        val subtreeChildren = subtree.getChildren
        if (subtreeChildren.nonEmpty & subtreeChildren.contains(head)) {
          val childTree = subtreeChildren(head)
          getSubtreeForAux(tail, childTree)
        } else None
      }
      case Nil => Some(subtree)
    }
  }

  /**
    * Smooths all distributions, i.e., assigns a small probability to symbols with zero probabilities and re-normalizes
    * distributions.
    *
    * @param gammaMin The smallest probability to be assigned.
    * @param allSymbols All symbols to be found in the tree.
    */
  def smoothDistributions(
                           gammaMin: Double,
                           allSymbols: Set[Symbol]
                         ): Unit = {
    dist = dist.smoothSer(gammaMin, allSymbols)
    if (children.nonEmpty) children.foreach(child => child._2.smoothDistributions(gammaMin, allSymbols))
  }

  /**
    * Checks if the leaves of the tree are "proper" for converting the tree into a suffix automaton.
    * Wherever (in whichever leaf) we are at any moment, by reading a new symbol, we must be able to jump to another
    * leaf. If this is the case, we can convert the tree leaves to automaton states and properly set their transitions,
    * since we know that we would never get stuck without knowing where to go next.
    *
    * @return True if the tree's transitions are proper.
    */
  def isTransitionProper: Boolean = {
    val leavesLabels = getLeaves.map(l => l.label)
    val symbols = getSymbols
    leavesLabels.forall(ll => symbols.forall(sigma => existsSuffixState(ll, sigma)))
  }

  /**
    * Checks if, for a given context (current suffix) and a given new symbol, the tree can move to a new leaf.
    * The new context (produced by appending at the beginning the new symbol to the old context) must lead us to a leaf.
    *
    * @param context The given context.
    * @param sigma The new symbol.
    * @return True if new symbol :: context leads us to a leaf.
    */
  private def existsSuffixState(
                                 context: List[Symbol],
                                 sigma: Symbol
                               ): Boolean = {
    val ssigma = sigma :: context
    val node = getNodeUntilLeafNonBlocking(ssigma)
    node.isLeaf
  }

  /**
    * Compares this tree against another. For two trees to be "equal", the following conditions must hold:
    *   they must have the same structure, i.e., nodes with the same labels that have children with the same labels;
    *   and for two corresponding nodes, their next symbol distributions must be "close" enough, according to the given
    *   margin.
    *
    * @param other The other tree.
    * @param margin The given margin.
    * @return True if the trees are "equal".
    */
  def compare(
               other: PredictionSuffixTree,
               margin: Double
             ): Boolean = {
    if (this.label != other.label) false
    else if (!this.dist.compare(other.dist, margin)) false
    else if (this.children.keySet != other.children.keySet) false
    else {
      if (this.children.isEmpty) true
      else children.forall(c => {
        val thatSubtree = other.children(c._1)
        val thisSubtree = this.children(c._1)
        thisSubtree.compare(thatSubtree, margin)
      })
    }
  }

  /**
    * @return The node's next symbol distribution.
    */
  def getDistribution: SymbolDistribution = dist

  /**
    * @return The total number of nodes in the tree.
    */
  def getSize: Long = getSizeAux(List(this))

  /**
    * @return The tree's leaves as a set of trees/nodes.
    */
  def getLeaves: Set[PredictionSuffixTree] = findLeaves(List(this), Set.empty)

  /**
    * @return All symbols present in the tree.
    */
  def getSymbols: Set[Symbol] = findAllSymbols(List(this), Set.empty[Symbol])

  /**
    * Estimates the average log-loss of a sequence.
    * l(P,X) = - (1/T)sum_{i=1,T}{log(P(x_{i}|x_{1}...x_{i-1}))}
    *
    * @param word The sequence given as a list of symbols. CAUTION: head of list must be the most recent/
    *             last in sequence symbol.
    * @return The log-loss.
    */
  def avgLogLoss(word: List[Symbol]): Double = {
    val logeval = logEval(word)
    logeval / word.length
  }

  /**
    * Estimates - sum_{i=1,T}{log(P(x_{i}|x_{1}...x_{i-1}))} for a sequence.
    *
    * @param word The sequence given as a list of symbols. CAUTION: head of list must be the most recent/
    *             last in sequence symbol.
    * @return - sum_{i=1,T}{log(P(x_{i}|x_{1}...x_{i-1}))}
    */
  def logEval(word: List[Symbol]): Double = {
    val sum = logEvalAux(word, 0.0)
    val result = -sum
    result
  }

  /**
    * Auxiliary recursive function to estimate - sum_{i=1,T}{log(P(x_{i}|x_{1}...x_{i-1}))}.
    *
    * @param word The remaining sequence given as a list of symbols.
    * @param accumulator The currently accumulated sum.
    * @return - sum_{i=1,T}{log(P(x_{i}|x_{1}...x_{i-1}))}
    */
  @scala.annotation.tailrec
  private def logEvalAux(
                          word: List[Symbol],
                          accumulator: Double
                        ): Double = {
    word match {
      case Nil => accumulator
      case head :: tail => {
        val prob = getConditionalProbFor(head, tail)
        val logProb = MathUtils.logbase(prob, 2)
        val newAccumulator = logProb + accumulator
        logEvalAux(tail, newAccumulator)
      }
    }
  }

  /**
    * Retrieves the probability for a symbol.
    *
    * @param symbol The given symbol.
    * @return The probability.
    */
  def getProbFor(symbol: Symbol): Double = dist.getProbFor(symbol)

  /**
    * Estimates the probability of a sequence of symbols.
    *
    * @param word The sequence given as a list of symbols (head is most recent).
    * @return The sequence probability.
    */
  def getProbFor(word: List[Symbol]): Double = getProbForAux(word, 1.0)

  /**
    * Auxiliary recursive function to estimate sequence probabilities.
    * The sequence probability is the product of conditional probabilities:
    * P(x_{k}|x_{1}...x_{k-1}) * P(x_{k-1}|x_{1}...x_{k-2}) * ...
    *
    * @param word The remaining sequence part.
    * @param accumulator The accumulated probability.
    * @return The sequence probability.
    */
  @scala.annotation.tailrec
  private def getProbForAux(
                             word: List[Symbol],
                             accumulator: Double
                           ): Double = {
    word match {
      case Nil => accumulator
      case head :: tail => {
        val prob = getConditionalProbFor(head, tail)
        val newAccumulator = prob * accumulator
        getProbForAux(tail, newAccumulator)
      }
    }
  }

  /**
    * Estimates the conditional probability of a symbol given a context.
    *
    * @param symbol The symbol.
    * @param context The context as a list of symbols (head is most recent).
    * @return The conditional probability.
    */
  def getConditionalProbFor(
                             symbol: Symbol,
                             context: List[Symbol]
                           ): Double = {
    val node = getNodeUntilLeafNonBlocking(context)
    val prob = node.dist.getProbFor(symbol)
    prob
  }

  /**
    * Estimates the conditional probability of a sequence/word given a context.
    * Product of probabilities:
    * P(x_{k}| context,x_{m}...x_{k-1}) * P(x_{k-1}|context,x_{m}...x_{k-2}) * ... * P(x_{m}|context)
    *
    * @param word The sequence as a list of symbols (head is most recent).
    * @param context The context as a list of symbols (head is most recent).
    * @return The conditional probability.
    */
  def getConditionalProbFor(
                             word: List[Symbol],
                             context: List[Symbol]
                           ): Double = getConditionalProbForAux(word, context, 1.0)

  /**
    * Auxiliary recursive function for
    * model.vmm.pst.PredictionSuffixTree#getConditionalProbFor(model.vmm.Symbol, scala.collection.immutable.List).
    *
    * @param word The remaining sequence.
    * @param context The context.
    * @param accumulator The accumulator.
    * @return The probability.
    */
  @scala.annotation.tailrec
  private def getConditionalProbForAux(
                                        word: List[Symbol],
                                        context: List[Symbol],
                                        accumulator: Double
                                      ): Double = {
    word match {
      case Nil => accumulator
      case head :: tail => {
        val thisContext = tail ::: context
        val prob = getConditionalProbFor(head, thisContext)
        val newAccumulator = prob * accumulator
        getConditionalProbForAux(tail, context, newAccumulator)
      }
    }
  }

  /**
    * @return A PST equivalent to this PST but which can be converted to a PSA.
    */
  def makePSACompatible(): PredictionSuffixTree = {
    val leaves = findLeaves(List(this), Set.empty).toList
    val symbols = findAllSymbols(List(this), Set.empty)
    makePSACompatibleAux(leaves, symbols)
  }

  /**
    * Auxiliary recursive function for model.vmm.pst.PredictionSuffixTree#makePSACompatible().
    * We first check the leaves. For every leaf we must be able to reach another leaf with any new symbol.
    * If for some leaf there exists a symbol with which we do not reach a leaf, then we expand this leaf one level down
    * and adds these new children as leaves to be checked. We keep doing this until all leaves are proper.
    *
    * @param remainingLeaves The leaves remaining to checked.
    * @param symbols All symbols present in the tree.
    * @return The new PST, convertible to a PSA.
    */
  @scala.annotation.tailrec
  private def makePSACompatibleAux(
                                    remainingLeaves: List[PredictionSuffixTree],
                                    symbols: Set[Symbol]
                                  ): PredictionSuffixTree = {
    if (remainingLeaves.isEmpty) this
    else {
      val leafToCheck = remainingLeaves.head
      val needsExpansion = symbols.exists(symbol => !checkIfSigmaContextReachesLeaf(leafToCheck.label, symbol))
      val newLeaves = if (needsExpansion) leafToCheck.expand(symbols) else List.empty
      val newRemainingLeaves = newLeaves ::: remainingLeaves.tail
      makePSACompatibleAux(newRemainingLeaves, symbols)
    }
  }

  /**
    * Expands this node with new children. For every child, its symbol distribution is copied from this node.
    * Assumes this node is a leaf.
    *
    * @param symbols All symbols present in the tree.
    * @return The newly created children.
    */
  def expand(symbols: Set[Symbol]): List[PredictionSuffixTree] = {
    require(children.isEmpty)
    val newChildren = symbols.toList.map(symbol => (symbol, PredictionSuffixTree(label ::: List(symbol), dist))).toMap
    children = newChildren
    newChildren.values.toList
  }

  /**
    * Checks if, for a given context (current suffix) and a given new symbol, the tree can move to a new leaf.
    * The new context (produced by appending at the beginning the new symbol to the old context) must lead us to a leaf.
    * Same as model.vmm.pst.PredictionSuffixTree#existsSuffixState(scala.collection.immutable.List, model.vmm.Symbol),
    * but uses blocking version of getNodeUntilLeaf.
    *
    * @param context The given context.
    * @param sigma The new symbol.
    * @return True if new symbol :: context leads us to a leaf.
    */
  private def checkIfSigmaContextReachesLeaf(
                                              context: List[Symbol],
                                              sigma: Symbol
                                            ): Boolean = {
    val labelToCheck = sigma :: context
    val reachedNode = getNodeUntilLeafBlocking(labelToCheck)
    reachedNode.isLeaf
  }

  /**
    * @return True if the node is a leaf, i.e., has no children.
    */
  def isLeaf: Boolean = children.isEmpty

  /**
    * Traverses the tree with a given word. Goes as far down as possible. If a leaf is reached (e.g., if the word has
    * greater length than the tree's order), stops there and returns this leaf. If it reaches a node without exhausting
    * the word (the current node has no child for the next symbol), it throws exception.
    *
    * @param word The given word.
    * @return The node reached.
    */
  def getNodeUntilLeafBlocking(word: List[Symbol]): PredictionSuffixTree = getNodeUntilLeafBlockingAux(word, this)

  /**
    * Auxiliary recursive function for
    *model.vmm.pst.PredictionSuffixTree#getNodeForUntilLeadBlocking(scala.collection.immutable.List).
    *
    * @param word The given word.
    * @param node The current node.
    * @return The node reached.
    */
  @scala.annotation.tailrec
  private def getNodeUntilLeafBlockingAux(
                                           word: List[Symbol],
                                           node: PredictionSuffixTree
                                         ): PredictionSuffixTree = {
    word match {
      case head :: tail => {
        val nodeChildren = node.getChildren
        if (nodeChildren.isEmpty) node
        else {
          require(nodeChildren.contains(head))
          val childFollowed = node.getChildren(head)
          getNodeUntilLeafBlockingAux(tail, childFollowed)
        }
      }
      case Nil => node
    }
  }

  /**
    * Traverses the tree with a given word. Goes as far down as possible. If a leaf is reached (e.g., if the word has
    * greater length than the tree's order), stops there and returns this leaf. If it reaches a node without exhausting
    * the word (the current node has no child for the next symbol), it returns this node.
    *
    * @param word The given word.
    * @return The node reached.
    */
  def getNodeUntilLeafNonBlocking(word: List[Symbol]): PredictionSuffixTree =
    getNodeUntilLeafNonBlockingAux(word, this)

  /**
    * Auxiliary recursive function for
    * model.vmm.pst.PredictionSuffixTree#getNodeForUntilLeaf(scala.collection.immutable.List).
    *
    * @param word The given word.
    * @param node The current node.
    * @return The node reached.
    */
  @scala.annotation.tailrec
  private def getNodeUntilLeafNonBlockingAux(
                                              word: List[Symbol],
                                              node: PredictionSuffixTree
                                            ): PredictionSuffixTree = {
    word match {
      case head :: tail => {
        val nodeChildren = node.getChildren
        if (nodeChildren.isEmpty) node
        else if (!nodeChildren.contains(head)) node
        else {
          val childFollowed = node.getChildren(head)
          getNodeUntilLeafNonBlockingAux(tail, childFollowed)
        }
      }
      case Nil => node
    }
  }

  /**
    * @return Number of leaves.
    */
  def getNoOfLeaves: Int = findLeaves(List(this), Set.empty).size

  /**
    * @return A set with all the labels in the tree.
    */
  def getAllLabels: Set[List[Symbol]] = getAllLabelsAux(List(this), Set.empty)

  /**
    * Auxiliary recursive function for model.vmm.pst.PredictionSuffixTree#getAllLabels().
    *
    * @param trees The reamining nodes to be checked.
    * @param accumulator The accumulator for labels.
    * @return The set of labels.
    */
  @scala.annotation.tailrec
  private def getAllLabelsAux(
                               trees: List[PredictionSuffixTree],
                               accumulator: Set[List[Symbol]]
                             ): Set[List[Symbol]] = {
    trees match {
      case head :: tail => {
        val newAcc: Set[List[Symbol]] = accumulator + head.label
        val newTrees: List[PredictionSuffixTree] = tail ::: head.getChildrenTrees
        getAllLabelsAux(newTrees, newAcc)
      }
      case Nil => accumulator
    }
  }

  /**
    * Finds all the leaves of the given trees. Traverses all given trees until reaching nodes with no children.
    *
    * @param pstl The given trees.
    * @param accumulator The accumulator of leaves/
    * @return The leaves, as a set of trees/nodes.
    */
  @scala.annotation.tailrec
  private def findLeaves(
                          pstl: List[PredictionSuffixTree],
                          accumulator: Set[PredictionSuffixTree]
                        ): Set[PredictionSuffixTree] = {
    pstl match {
      case head :: tail => {
        if (head.getChildren.isEmpty) {
          val newAcc: Set[PredictionSuffixTree] = accumulator + head
          findLeaves(tail, newAcc)
        } else {
          val newPstl = tail ::: head.getChildrenTrees
          findLeaves(newPstl, accumulator)
        }
      }
      case Nil => accumulator
    }
  }

  /**
    * Finds all the symbols present in the given trees.
    *
    * @param pstl The given trees.
    * @param accumulator The accumularot with all found symbols.
    * @return set with all found symbols.
    */
  @scala.annotation.tailrec
  private def findAllSymbols(
                              pstl: List[PredictionSuffixTree],
                              accumulator: Set[Symbol]
                            ): Set[Symbol] = {
    pstl match {
      case head :: tail => {
        val newAcc: Set[Symbol] = accumulator ++ head.label.toSet
        val remaining = tail ::: head.getChildrenTrees
        findAllSymbols(remaining, newAcc)
      }
      case Nil => accumulator
    }
  }

  /**
    * @return The nodes of the children.
    */
  def getChildrenTrees: List[PredictionSuffixTree] = {
    val q = children.toSeq.sortWith(_._1.value < _._1.value)
    q.map(x => x._2).toList
  }

  /**
    * @return A map from symbols to children nodes.
    */
  def getChildren: Map[Symbol, PredictionSuffixTree] = children

  /**
    * Counts the total number of nodes for the given trees.
    *
    * @param pstl The given trees.
    * @return The total number of nodes from all the given trees.
    */
  private def getSizeAux(pstl: List[PredictionSuffixTree]): Long = {
    if (pstl.isEmpty) 0
    else {
      val allChildren = pstl.flatMap(x => x.getChildrenTrees)
      pstl.size + getSizeAux(allChildren)
    }
  }

  override def toString: String = tree2str(List((this, List.empty)), "")

  /**
    * @return The node's label as a string.
    */
  private def nodeStr: String = "(" + StringUtils.list2Str(label) + "," + dist + ")"

  /**
    * Auxiliary recursive function to stringify a tree. Each level in its own line.
    *
    * @param pst The current list of trees remaining to be stringified.
    * @param str The current string accumulator.
    * @return The tree as a string.
    */
  @scala.annotation.tailrec
  private def tree2str(
                        pst: List[(PredictionSuffixTree, List[Symbol])],
                        str: String
                      ): String = {
    pst match {
      case Nil => str
      case _ => {
        val (frontStr, newFront) = tree2strAux(pst, "", List.empty[(PredictionSuffixTree, List[Symbol])])
        tree2str(newFront, str + "\n" + frontStr)
      }
    }
  }

  /**
    * Auxiliary recursive function to stringify a tree. Used to create a string from all the nodes at a certain level
    * and also to return all the nodes of the next level.
    *
    * @param front The nodes of the current level.
    * @param str The string accumulator as we process more nodes at the current level.
    * @param newFront The children of all current level nodes.
    * @return The string of the current level, along with all the children of the current level nodes.
    */
  @scala.annotation.tailrec
  private def tree2strAux(
                           front: List[(PredictionSuffixTree, List[Symbol])],
                           str: String,
                           newFront: List[(PredictionSuffixTree, List[Symbol])]
                         ): (String, List[(PredictionSuffixTree, List[Symbol])]) = {
    front match {
      case head :: tail => tree2strAux(tail, str + "|" + head._1.nodeStr + "<-" + head._2, newFront ::: (head._1.getChildrenTrees.map(x => (x, head._1.label))))
      case Nil => (str, newFront)
    }
  }

  /**
    * Prints the tree and converts while converting each node symbol to its corresponding minterm, as defined in the
    * given isomorphism.
    *
    * @param iso The given isomorphism.
    * @param pMin
    */
  def print(
             iso: Isomorphism,
             pMin: Double
           ): Unit = {
    printAux(iso, pMin)
  }

  /**
    * Auxiliary recursive function to print the tree according to a given isomorphism. Symbols replaced by sequences
    * of minterms.
    *
    * @param iso The given isomorphism.
    * @param pMin Threshold to reject symbols when printing distributions.
    */
  private def printAux(
                        iso: Isomorphism,
                        pMin: Double
                      ): Unit = {
    val thisPathLabel = label
    val thisPathLabelStr = thisPathLabel.map(s => {
      if (iso.getSymbols.contains(s)) iso.getMinTermForSymbol(s).toString
      else s.toString
    })
    val str = "(" + thisPathLabelStr + "," + dist.toString(pMin) + ")\n"
    println(str)
    children.values.foreach(child => child.printAux(iso, pMin))
  }

}
