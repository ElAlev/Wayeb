package fsm.classical.pattern.regexp.archived

import fsm.classical.pattern.regexp.NodeType._
import fsm.classical.pattern.regexp.OperatorType._

class Node(
    nodeType: NodeType,
    operator: OperatorType,
    symbol: String
) {
  require((nodeType == OPERATOR & symbol == "") | (nodeType == SYMBOL & operator == NONE))

  private var children = List[Node]()

  def addChildren(ch: List[Node]): Unit = {
    require(nodeType == OPERATOR)
    require((operator == ITER & ch.size == 1) | ((operator == CONCAT | operator == UNION) & ch.size == 2))
    children = ch
  }

  def getLeftChild: Node = {
    require(nodeType == OPERATOR)
    children.head
  }

  def getRightChild: Node = {
    require(nodeType == OPERATOR)
    require(operator == CONCAT | operator == UNION)
    children(1)
  }

  def isLeaf: Boolean = children.isEmpty

  override def toString: String = {
    if (nodeType == OPERATOR) operator.toString
    else symbol
  }

  def toStringRecursively: String = {
    var s = ""
    s += toString
    if (nodeType == OPERATOR) {
      s += "("
      if (operator == ITER) s += getLeftChild.toStringRecursively
      else {
        s += getLeftChild.toStringRecursively
        s += getRightChild.toStringRecursively
      }
      s += ")"
    } else {
      s += ","
    }
    //if (nodeType==OPERATOR) s += ")"
    s
  }
}
