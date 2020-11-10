package fsm.classical.pattern.regexp.archived

import fsm.classical.pattern.regexp.NodeType._
import fsm.classical.pattern.regexp.OperatorType._
import fsm.classical.pattern.regexp.archived
import scala.collection.mutable
import scala.xml._

class REReader(filename: String) {
  private val symbols = mutable.Set[String]()

  private def getRETree: archived.Node = {
    val loadnode = XML.loadFile(filename)
    val regexp = loadnode \\ "regexp"
    val rootnodexml = (regexp \ "node").head
    readNodeRecursively(rootnodexml)

  }

  def getRETreeSigmaStar: archived.Node = {
    val ret = getRETree
    val rootNode = new archived.Node(OPERATOR, CONCAT, "")
    val iterNode = new archived.Node(OPERATOR, ITER, "")
    var currentSymbol = 0
    var prevNode: archived.Node = null
    for (symbol <- symbols) {
      currentSymbol += 1
      val symNode = new archived.Node(SYMBOL, NONE, symbol)
      if (currentSymbol == 1) {
        prevNode = symNode
      } else {
        val unionNode = new archived.Node(OPERATOR, UNION, "")
        unionNode.addChildren(List(prevNode, symNode))
        prevNode = unionNode
      }
    }
    iterNode.addChildren(List(prevNode))
    rootNode.addChildren(List(iterNode, ret))
    rootNode
  }

  private def readNodeRecursively(n: scala.xml.Node): archived.Node = {
    var nodeType = OPERATOR
    var operator = NONE
    var symbol = ""
    val nodeTypeStr = (n \ "@type").text
    if (nodeTypeStr.equalsIgnoreCase("operator")) {
      val operatorType = (n \ "operator").text
      if (operatorType.equalsIgnoreCase("concat")) operator = CONCAT
      else if (operatorType.equalsIgnoreCase("union")) operator = UNION
      else if (operatorType.equalsIgnoreCase("iter")) operator = ITER
      else throw new IllegalArgumentException("Operator type can only be concat, union or iter")
      //println((nodeTypeStr+":"+operator))
    } else if (nodeTypeStr.equalsIgnoreCase("symbol")) {
      symbol = (n \ "symbol").text
      if (symbol.length() != 1) throw new IllegalArgumentException("Only chars allowed as symbols for now")
      if (symbol(0) == '$') throw new IllegalArgumentException(" Char $ is reserved for epsilon transitions")
      symbols.add(symbol)
      nodeType = SYMBOL
      //println((nodeTypeStr+":"+symbol))
    } else throw new IllegalArgumentException("Pattern can only have nodes that are either operators or symbols")
    val reNode = new archived.Node(nodeType, operator, symbol)
    val children = n \ "node"
    var reChildren = List[archived.Node]()
    for (child <- children) {
      val newReNode: archived.Node = readNodeRecursively(child)
      reChildren = reChildren ::: List(newReNode)
    }
    if (reChildren.nonEmpty) reNode.addChildren(reChildren)
    reNode

  }
}
