package utils.testing

import breeze.stats.distributions._
import fsm.classical.pattern.regexp.NodeType._
import fsm.classical.pattern.regexp.OperatorType._
import fsm.classical.pattern.regexp.{OperatorNode, RegExpTree, RegExpUtils, SymbolNode}
import scala.collection.mutable

/**
  * Utils for randomly generating regular expression patterns.
  */
object PatternGenerator {

  /**
    * Randomly generates total number of regular expression trees from a given set of symbols.
    * The maximum depth of each tree is maxDepth.
    *
    * @param total The total number of regular expression trees to be generated.
    * @param symbols The set of symbols.
    * @param maxDepth The maximum depth of each tree.
    * @return The set of regular expression trees.
    */
  def generateRegExpPatterns(
                              total: Int,
                              symbols: Set[String],
                              maxDepth: Int
                            ): Set[RegExpTree] = {
    val patterns = mutable.Set[RegExpTree]()
    (1 to total).foreach(_ => {
      val newPattern = genRegExpPattern(0, maxDepth, symbols.toList, NONE)
      if (RegExpUtils.leastSymbolsNo(newPattern) != 0) patterns.add(newPattern)
    })
    patterns.toSet
  }

  /**
    * Recursively generates a  regular expression tree from a given set of symbols of depth up to maxDepth.
    * For every node, first choose whether a symbol or an operator is to be added. If symbol, create the node and stop.
    * If operator, randomly choose the operator and move to its children.
    *
    * @param currentDepth The current depth of the tree.
    * @param maxDepth The maximum depth of the tree.
    * @param symbols The set of symbols.
    * @param previousOp The operator chosen at the previous level (required to avoid multiple, nested star operators).
    * @return The regular expression tree.
    */
  private def genRegExpPattern(
                                currentDepth: Int,
                                maxDepth: Int,
                                symbols: List[String],
                                previousOp: OperatorType
                              ): RegExpTree = {
    if (currentDepth == maxDepth) SymbolNode(sampleSymbol(symbols))
    else {
      val nodeType = sampleNodeType()
      nodeType match {
        case SYMBOL => SymbolNode(sampleSymbol(symbols))
        case OPERATOR => {
          val op = sampleOperator(previousOp)
          op match {
            case ITER => OperatorNode(ITER, List(genRegExpPattern(currentDepth + 1, maxDepth, symbols, op)))
            case _ => OperatorNode(op, List(genRegExpPattern(currentDepth + 1, maxDepth, symbols, op), genRegExpPattern(currentDepth + 1, maxDepth, symbols, op)))
          }
        }
      }
    }
  }

  /**
    * Randomly picks a symbol for a given set.
    *
    * @param symbols The given set of symbols.
    * @return The random symbol chosen.
    */
  private def sampleSymbol(symbols: List[String]): String = {
    val uni = new Uniform(0, symbols.size)
    val s = uni.sample().toInt
    symbols(s)
  }

  /**
    * @return A random choice between symbol and operator.
    */
  private def sampleNodeType(): NodeType = {
    val uni = new Uniform(0, 1)
    val s = uni.sample()
    if (s <= 0.5) SYMBOL
    else OPERATOR
  }

  /**
    * Randomly chooses one operator. Avoids nested iterations.
    *
    * @param previousOp The operator at the previous level.
    * @return The new operator.
    */
  private def sampleOperator(previousOp: OperatorType): OperatorType = {
    var op = sampleOperator()
    if (previousOp == ITER) {
      while (op == ITER) op = sampleOperator()
    }
    op
  }

  /**
    * @return A random choice for an operator.
    */
  private def sampleOperator(): OperatorType = {
    val uni = new Uniform(0, 3)
    val s = uni.sample()
    if (s <= 1.0) CONCAT
    else if (s <= 2.0) UNION
    else ITER
  }

}
