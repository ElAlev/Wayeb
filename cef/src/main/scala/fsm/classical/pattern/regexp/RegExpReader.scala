package fsm.classical.pattern.regexp

import scala.xml._
import fsm.classical.pattern.regexp.OperatorType._

/**
  * A reader for regular expressions. Regular expressions are written as xml files.
  * Each pattern has three nodes: the pattern's name, the partition attribute and the regular expression itself.
  * The regular expression must be written as a tree of operators. concat and union operators are binary. iter operators
  * are unary.
  * See the example below:
  * <pattern>
  *   <name>abc</name>
  *   <partitionAttribute>$</partitionAttribute>
  *   <regexp>
  *     <node type="operator">
  *     <operator>concat</operator>
  *       <node type="symbol">
  *       <symbol>A</symbol>
  *       </node>
  *       <node type="operator">
  *       <operator>concat</operator>
  *         <node type="symbol">
  *         <symbol>B</symbol>
  *         </node>
  *         <node type="symbol">
  *         <symbol>C</symbol>
  *         </node>
  *       </node>
  *     </node>
  *   </regexp>
  * </pattern>
  *
  *
  * @param filename The path to the xml file.
  * @param sigma The alphabet provided as a set of symbols.
  */
class RegExpReader(
    filename: String,
    sigma: Set[String]
) {

  /**
    * @return The tree of the regular expression along with the partition attribute.
    */
  def getRegExpTree: (RegExpTree, String) = {
    val loadnode = XML.loadFile(filename)
    val regexp = loadnode \\ "regexp"
    val partAttr = (loadnode \\ "partitionAttribute").text
    val rootnodexml = (regexp \ "node").head
    val ret = readNodeRecursively(rootnodexml)
    (ret, partAttr)
  }

  /**
    * @return The tree of the streaming regular expression along wih the partition attribute.
    */
  def getRegExpTreeWithSigmaStar: (RegExpTree, String) = {
    val retAndPart = getRegExpTree
    val ret = retAndPart._1
    val part = retAndPart._2
    val res = OperatorNode(CONCAT, List(getSigmaStarTree(sigma.toList), ret))
    (res, part)
  }

  /**
    * Constructs the tree corresponding to the iteration of the union of all the provided symbols.
    *
    * @param symbols The given symbols.
    * @return The tree.
    */
  private def getSigmaStarTree(symbols: List[String]): RegExpTree = OperatorNode(ITER, List(getSigmaUnion(symbols)))

  /**
    * Constructs the tree corresponding to the union of all provided symbols.
    *
    * @param symbols The given symbols.
    * @return The tree of their union.
    */
  private def getSigmaUnion(symbols: List[String]): RegExpTree = {
    if (symbols.size == 1) SymbolNode(symbols.head)
    else OperatorNode(UNION, List(SymbolNode(symbols.head), getSigmaUnion(symbols.tail)))
  }

  /**
    * Converts an xml node to a regular expression tree. Works recursively.
    * Calls fsm.classical.pattern.regexp.RegExpReader#readNodesRecursively(scala.collection.immutable.List)
    * which then calls again fsm.classical.pattern.regexp.RegExpReader#readNodeRecursively(scala.xml.Node).
    *
    * @param xmlNode The xml node.
    * @return The regular expression tree.
    */
  private def readNodeRecursively(xmlNode: scala.xml.Node): RegExpTree = {
    val nodeTypeStr = (xmlNode \ "@type").text
    nodeTypeStr match {
      case "symbol" => SymbolNode((xmlNode \ "symbol").text)
      case "operator" =>
        val childrenNodes = (xmlNode \ "node").toList; OperatorNode(getOperatorType(xmlNode), readNodesRecursively(childrenNodes))
      case _ => throw new IllegalArgumentException("Pattern can only have nodes that are either operators or symbols")
    }
  }

  /**
    * Converts a list of xml nodes to a list of regular expression trees.
    *
    * @param xmlNodes The list of xml nodes.
    * @return The list of trees.
    */
  private def readNodesRecursively(xmlNodes: List[scala.xml.Node]): List[RegExpTree] = xmlNodes.map(x => readNodeRecursively(x))

  /**
    * Determines the type of the xml node.
    *
    * @param n The xml node to check.
    * @return CONCAT, UNION or ITER.
    */
  private def getOperatorType(n: scala.xml.Node): OperatorType = {
    val ot = (n \ "operator").text
    ot match {
      case "concat" => CONCAT
      case "union" => UNION
      case "iter" => ITER
      case _ => throw new IllegalArgumentException("Operator type can only be concat, union or iter")
    }
  }
}
