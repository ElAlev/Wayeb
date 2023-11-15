package model.vmm.pst

import model.vmm.Symbol
import model.vmm.mapper.Isomorphism

object CounterSuffixTree {
  /**
    * Constructor for counter suffix tree.
    *
    * @param cnt The initial node counter.
    * @param ch The initial list of children.
    * @param nodeSymbol The node's symbol.
    * @return the CST.
    */
  def apply(
             cnt: Int,
             ch: Map[Symbol, CounterSuffixTree],
             nodeSymbol: Symbol
           ): CounterSuffixTree = new CounterSuffixTree(cnt, ch, nodeSymbol)

  /**
    * Constructor for an empty counter suffix tree.
    *
    * @return An empty CST, i.e., a CST with a single node with counter=0 and symbol=epsilon.
    */
  def apply(): CounterSuffixTree = new CounterSuffixTree(0, Map.empty[Symbol, CounterSuffixTree], Symbol())

  /**
    * Constructor for a counter suffix tree with no children.
    *
    * @param cnt The initial node counter.
    * @param nodeSymbol the node's symbol.
    * @return The CST.
    */
  def apply(
             cnt: Int,
             nodeSymbol: Symbol
           ): CounterSuffixTree = new CounterSuffixTree(0, Map.empty[Symbol, CounterSuffixTree], nodeSymbol)
}

/**
  * Class representing a Counter Suffix Tree (CST).
  * Each  node in a CST is a tuple (σ, c) where σ is a symbol from the alphabet (or epsilon only for the root
  * node) and c a counter. The counter of every node is equal to the sum of the counters of its
  * children. By following a path from the root to a node, we get a string s = σ0 · σ1 · · · σn, where
  * σ0 = epsilon corresponds to the root node and σn to the symbol of the node that is reached. The
  * property that we maintain as we build a CST from a stream S1..k is that the counter of the
  * node that is reached with s gives us the number of occurrences of the string σn · σn−1 · · · σ1
  * (the reversed version of s) in S1..k.
  *
  * The property of a node's counter being equal to the sum of its children's counters holds if we build the tree
  * (by calling model.vmm.pst.CounterSuffixTree#updateWithNewWord(scala.collection.immutable.List))
  * with words of the same length. If we update the tree by also pushing words of smaller length (e.g., when using a
  * stream, the first words pushed to the tree will be of size 1, 2, etc until we reach the order m and after that all
  * words will be of size m), the property might not hold exactly. It must be slightly modified.
  * The root's (level 0) counter will be equal to the total number of events.
  * For the nodes at level 1, the sum of their counter will be equal to the sum of the root counter.
  * For the nodes at level 2, sumOfCounters = parentCounter or sumOfCounters = parentCounter - 1.
  * For the nodes at level 3, sumOfCounters <= parentCounter and sumOfCounters >= parentCounter - 2.
  * For the nodes at level k, sumOfCounters <= parentCounter and sumOfCounters >= parentCounter - (k-1).
  * The advantage of building a tree this way (by also pushing words of smaller length at the beginning of the stream)
  * is that the property of counters being the number of string occurrences holds now not only for strings of length m
  * but for strings of any length.
  *
  * @param cnt The initial node counter.
  * @param ch The initial list of children.
  * @param nodeSymbol The node's symbol.
  */
class CounterSuffixTree(
                         cnt: Int,
                         ch: Map[Symbol, CounterSuffixTree],
                         val nodeSymbol: Symbol
                       ) {
  private var counter: Int = cnt
  private var children: Map[Symbol, CounterSuffixTree] = ch
  // ignore root node that contains the empty symbol
  private var symbols = findAllSymbols(children.values.toList, Set.empty)

  /**
    * @return The number of nodes.
    */
  def getSize: Long = getSizeAux(List(this))

  /**
    * @return The node's current counter.
    */
  def getCounter: Int = counter

  /**
    * @return All symbols of the tree rooted at this node.
    */
  def getSymbols: Set[Symbol] = symbols

  /**
    * Auxiliary recursive function to find all symbols of a tree.
    *
    * @param trees The current list of nodes to examine.
    * @param accumulator The set of currently accumulated symbols.
    * @return The set of all symbols found in the given trees.
    */
  @scala.annotation.tailrec
  private def findAllSymbols(
                              trees: List[CounterSuffixTree],
                              accumulator: Set[Symbol]
                            ): Set[Symbol] = {
    trees match {
      case head :: tail => {
        val newAcc: Set[Symbol] = accumulator + head.nodeSymbol
        findAllSymbols(tail ::: children.values.toList, newAcc)
      }
      case Nil => accumulator
    }
  }

  /**
    * Auxiliary recursive function to find the size of the tree.
    *
    * @param trees The current list of nodes to examine.
    * @return The number of nodes of the given trees.
    */
  private def getSizeAux(trees: List[CounterSuffixTree]): Long = {
    if (trees.isEmpty) 0
    else {
      val allChildren = trees.flatMap(x => x.getChildrenTrees)
      trees.size + getSizeAux(allChildren)
    }
  }

  /**
    * Retrieves the counter for a word. Head of the word should be the last/most recent symbol. If the word does not
    * exist in the tree, 0 is returned.
    *
    * @param word The given word, as a list of symbols.
    * @return The word's counter.
    */
  def getCounterFor(word: List[Symbol]): Int = {
    val subtree = getSubtreeFor(word)
    //if (subtree == null) 0
    //else getSubtreeFor(word).getCounter
    subtree match {
      case Some(cst) => cst.getCounter
      case None => 0
    }
  }

  /**
    * Retrieves the node corresponding to a given word. Head of the word should be the last/most recent symbol.
    * If the word does not exist, None is returned.
    *
    * @param word The given word
    * @return The node or null if the word is not found.
    */
  private def getSubtreeFor(word: List[Symbol]): Option[CounterSuffixTree] = getSubtreeForAux(word, this)

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
                                subtree: CounterSuffixTree
                              ): Option[CounterSuffixTree] = {
    word match {
      case head :: tail => {
        val subtreeChildren = subtree.getChildren
        //require(subtreeChildren.nonEmpty, "Reached leaf without exhausting word.")
        if (subtreeChildren.nonEmpty & subtreeChildren.contains(head)) {
          val childTree = subtreeChildren(head)
          getSubtreeForAux(tail, childTree)
        } else None
      }
      case Nil => Some(subtree)
    }
  }

  /**
    * Adds a new word to the tree. We traverse the tree according to the word. Whenever an existing node is touched, we
    * increase its counter by one. If a node does not exist, it is created and its counter set to 1.
    *
    * @param word The word to be added.
    * @return The counter of the final node reached.
    */
  def updateWithNewWord(word: List[Symbol]): Int = {
    counter += 1
    word match {
      case head :: tail => {
        if (children.contains(head)) {
          // existing node
          val child = children(head)
          child.updateWithNewWord(tail)
        } else {
          // new node created
          symbols += head
          val newChild = addNewChild(head)
          newChild.updateWithNewWord(tail)
        }
      }
      case Nil => counter
    }
  }

  /**
    * Estimates the (unconditional) probability of a given word. Assumes the root counter corresponds to the total
    * number of symbols. Returns 0 if the word is does not exist or its length is greater than the tree's order.
    *
    * @param word The given word.
    * @return The word's probability.
    */
  def getProbFor(word: List[Symbol]): Double = getCounterFor(word).toDouble / (counter - word.length + 1)

  /**
    * Estimates the conditional probability of a symbol given a context.
    * TODO: Not totally correct. contextCounter may be higher than sum of sigmaContextCounters (e.g., when context at the end of stream)
    *
    * @param sigma The symbol.
    * @param context The context, as a list of symbols (head of list is the most recent symbol).
    * @return
    */
  def getConditionalProbFor(
                             sigma: Symbol,
                             context: List[Symbol]
                           ): Double = {
    val sigmaContextCounter = getCounterFor(sigma :: context)
    val contextCounter = getCounterFor(context)
    if (contextCounter == 0) {
      if (sigmaContextCounter != 0) throw new Error("invalid conditional probability")
      else 0.0
    }
    else sigmaContextCounter.toDouble / contextCounter
  }

  /**
    * Estimates the symbol distribution given a certain context.
    *
    * @param context The context.
    * @return The symbol distribution.
    */
  def getSymbolDistributionFor(context: List[Symbol]): SymbolDistribution = {
    val sigmas = symbols.toList
    val sigmaContextCounters = sigmas.map(x => (x, getCounterFor(x :: context)))
    val sum = sigmaContextCounters.map(x => x._2).sum
    val condProbs = sigmaContextCounters.map(x => (x._1, x._2.toDouble / sum)).toMap
    SymbolDistribution(condProbs)
    // Do not use getConditionalProbFor, might give inconsistent distributions
    //val d = sigmas.map(sigma => (sigma,getConditionalProbFor(sigma,suffix))).toMap
    //SymbolDistribution(d)
  }

  /**
    * Adds a new child to this node with a zero counter. Assumes that there does not already exist a child with the
    * given symbol.
    *
    * @param symbol The given symbol.
    * @return The tree/node of the new child.
    */
  private def addNewChild(symbol: Symbol): CounterSuffixTree = {
    require(!children.contains(symbol))
    val newChild: CounterSuffixTree = CounterSuffixTree(0, symbol)
    children += (symbol -> newChild)
    newChild
  }

  /**
    * @return All children nodes as a list of trees, sorted by node symbol.
    */
  def getChildrenTrees: List[CounterSuffixTree] = {
    val q = children.toSeq.sortWith(_._1.value < _._1.value)
    q.map(x => x._2).toList
  }

  /**
    * @return The symbols of all the children as a sorted list of symbols.
    */
  def getChildrenSymbols: List[Symbol] = children.keys.toSeq.sortWith(_.value < _.value).toList

  /**
    * @return the children as a map of symbols to trees.
    */
  def getChildren: Map[Symbol, CounterSuffixTree] = children

  /**
    * @return symbol and counter as a string.
    */
  private def nodeStr: String = "(" + nodeSymbol + "," + counter + ")"

  /**
    * @return tree as a string.
    */
  override def toString: String = tree2str(List((this, Symbol())), "")

  /**
    * Auxiliary recursive function to stringify a tree. Each level in its own line.
    *
    * @param cst The current list of trees remaining to be stringified.
    * @param str The current string accumulator.
    * @return The tree as a string.
    */
  @scala.annotation.tailrec
  private def tree2str(
                        cst: List[(CounterSuffixTree, Symbol)],
                        str: String
                      ): String = {
    cst match {
      case Nil => str
      case _ => {
        val (frontStr, newFront) = tree2strAux(cst, "", List.empty[(CounterSuffixTree, Symbol)])
        // now we have the string of the current level (frontStr)
        // we add it to the current accumulator (str) to get str + "\n" + frontStr
        // and we move on to the children of the current level's nodes (newFront)
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
                           front: List[(CounterSuffixTree, Symbol)],
                           str: String,
                           newFront: List[(CounterSuffixTree, Symbol)]
                         ): (String, List[(CounterSuffixTree, Symbol)]) = {
    front match {
      case head :: tail => tree2strAux(
        tail,
        str + "|" + head._1.nodeStr + "<-" + head._2,
        newFront ::: head._1.getChildrenTrees.map(x => (x, head._1.nodeSymbol))
      )
      case Nil => (str, newFront)
    }
  }

  /**
    * Prints the tree and converts while converting each node symbol to its corresponding minterm, as defined in the
    * given isomorphism.
    *
    * @param iso The given isomorphism.
    */
  def print(iso: Isomorphism): Unit = printAux(List.empty, iso)

  /**
    * Auxiliary recursive function to print the tree according to a given isomorphism. Symbols replaced by sequences
    * of minterms.
    *
    * @param pathLabel The current path as a list of symbols.
    * @param iso The given isomorphism.
    */
  private def printAux(
                        pathLabel: List[Symbol],
                        iso: Isomorphism
                      ): Unit = {
    val thisPathLabel = pathLabel ::: List(nodeSymbol)
    val thisPathLabelStr = thisPathLabel.map(s => {
      if (iso.getSymbols.contains(s)) iso.getMinTermForSymbol(s).toString
      else s.toString
    })
    val str = "(" + thisPathLabelStr + "," + counter + ")\n"
    println(str)
    children.values.foreach(child => child.printAux(thisPathLabel, iso))
  }

  /**
    * Checks whether each node counter is equal to the sum of its children's counter. Should be used only when the tree
    * is constructed by pushing to it words of the same length.
    *
    * @return True if the tree is consistent.
    */
  def consistencyCheck: Boolean = {
    if (children.isEmpty) true
    else {
      val childrenSum = children.map(x => x._2.getCounterFor(List.empty)).sum
      if (counter != childrenSum) false
      else children.forall(x => x._2.consistencyCheck)
    }
  }

  //TODO: another consistency check for cases where the tree is constructed from a stream.

}
