package fsm.classical.pattern.regexp

import fsm.classical.pattern.regexp.OperatorType._

/**
  * A regular expression is represented as a tree.
  */
abstract class RegExpTree

/**
  * Each internal node of the tree is an operator.
  *
  * @param operator The operator type of the node, ITER, UNION or CONCAT.
  * @param children The sub-expressions of the node.
  */
case class OperatorNode(
                         operator: OperatorType,
                         children: List[RegExpTree]
                       ) extends RegExpTree {
  require((operator == ITER & children.size == 1) | (children.size == 2), "ITER must have only a single child. UNION and CONCAT only two.")

  override def toString: String = operator.toString + "(" + childrenAsString + ")"

  /**
    * @return The node's sub-expressions as a string.
    */
  private def childrenAsString: String = {
    if (operator == ITER) children.head.toString
    else children.head.toString + children(1).toString
  }
}

/**
  * Each leaf of the tree is a terminal symbol.
  *
  * @param symbol The node's symbol, as a string.
  */
case class SymbolNode(symbol: String) extends RegExpTree {
  override def toString: String = symbol
}
