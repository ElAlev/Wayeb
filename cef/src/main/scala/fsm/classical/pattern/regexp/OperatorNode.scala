package fsm.classical.pattern.regexp

import fsm.classical.pattern.regexp.OperatorType._

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
