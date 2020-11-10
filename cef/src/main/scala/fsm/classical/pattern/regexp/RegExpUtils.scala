package fsm.classical.pattern.regexp

import java.nio.file.{Files, Paths}
import fsm.classical.pattern.regexp.OperatorType.{CONCAT, ITER, UNION}

class RegExpUtils {

}

/**
  * Utils for handling regular expressions.
  */
object RegExpUtils {

  /**
    * Constructs the streaming regular expression tree from a pattern given in an xml file and the alphabet symbols.
    *
    * @param fn The path to the xml file.
    * @param streamSymbols The alphabet symbols.
    * @return The streaming tree, along with the partition attribute.
    */
  def xml2re(
              fn: String,
              streamSymbols: Set[String]
            ): (RegExpTree, String) = {
    require(Files.exists(Paths.get(fn)))
    val rer = new RegExpReader(fn, streamSymbols.map(x => x).toList.toSet)
    val patternREPart = rer.getRegExpTreeWithSigmaStar
    val patternRE = patternREPart._1
    val partitionAttribute = patternREPart._2
    require(RegExpUtils.leastSymbolsNo(patternRE) != 0, "Degenerate expression")
    (patternRE, partitionAttribute)
  }

  /**
    * Constructs a streaming regular expression tree from a given tree and all the symbols of the alphabet.
    *
    * @param initPattern The initial, given tree.
    * @param allSymbols The alphabet symbols.
    * @return The streaming tree.
    */
  def getPatternSigmaStar(
                           initPattern: RegExpTree,
                           allSymbols: Set[String]
                         ): RegExpTree = {
    println(allSymbols)
    val sigmaStarPattern = RegExpUtils.getIterRE(RegExpUtils.getUnionStr(allSymbols.toList))
    val pattern = RegExpUtils.getConcatRE(List(sigmaStarPattern, initPattern))
    pattern
  }

  /**
    * Constructs a regular expression tree corresponding to the union of the given symbols.
    *
    * @param symbols The given symbols.
    * @return The tree.
    */
  def getUnionStr(symbols: List[String]): RegExpTree = {
    if (symbols.size == 1) SymbolNode(symbols.head)
    else OperatorNode(UNION, List(SymbolNode(symbols.head), getUnionStr(symbols.tail)))
  }

  /**
    * Constructs a regular expression tree corresponding to the concatenation of the given symbols.
    *
    * @param symbols the given symbols.
    * @return The tree.
    */
  def getConcatStr(symbols: List[String]): RegExpTree = {
    if (symbols.size == 1) SymbolNode(symbols.head)
    else OperatorNode(CONCAT, List(SymbolNode(symbols.head), getConcatStr(symbols.tail)))
  }

  /**
    * Constructs a regular expression tree corresponding to the iteration of the given symbol.
    *
    * @param symbol The given symbol.
    * @return The tree.
    */
  def getIterStr(symbol: String): RegExpTree = OperatorNode(ITER, List(SymbolNode(symbol)))

  /**
    * Constructs a regular expression tree corresponding to the union of the given subtrees.
    *
    * @param regExpTrees The given subtrees.
    * @return The final tree.
    */
  private def getUnionRE(regExpTrees: List[RegExpTree]): RegExpTree = {
    if (regExpTrees.size == 1) regExpTrees.head
    else OperatorNode(UNION, List(regExpTrees.head, getUnionRE(regExpTrees.tail)))
  }

  /**
    * Constructs a regular expression tree corresponding to the concatenation of the given subtrees.
    *
    * @param regExpTrees The given subtrees.
    * @return The final tree.
    */
  private def getConcatRE(regExpTrees: List[RegExpTree]): RegExpTree = {
    if (regExpTrees.size == 1) regExpTrees.head
    else OperatorNode(CONCAT, List(regExpTrees.head, getConcatRE(regExpTrees.tail)))
  }

  /**
    * Constructs a regular expression tree corresponding to the iteration of the given subtree.
    *
    * @param regExpTree The given subtree.
    * @return The final tree.
    */
  private def getIterRE(regExpTree: RegExpTree): RegExpTree = OperatorNode(ITER, List(regExpTree))

  /**
    * Finds the least number of symbols that a given regular expression tree can have. Essentially finds the smallest
    * length that a word accepted by the expression can have.
    *
    * @param pattern The given regular expression tree.
    * @return The least number of symbols the tree can have.
    */
  def leastSymbolsNo(pattern: RegExpTree): Int = {
      def min(x: Int, y: Int): Int = if (x < y) x else y
    pattern match {
      // for a single symbol, exactly 1 is the number of symbols
      case SymbolNode(_) => 1
      // iteration also accepts the empty word, so 0 is the least number
      case OperatorNode(ITER, _) => 0
      // for union, we must find the minimum number of symbols from all subexpressions
      case OperatorNode(UNION, List(x, y)) => min(leastSymbolsNo(x), leastSymbolsNo(y))
      // for concatenation, we add the minimum number of symbols from all subexpressions
      case OperatorNode(CONCAT, List(x, y)) => leastSymbolsNo(x) + leastSymbolsNo(y)
      case _ => throw new IllegalArgumentException("invalid expression: " + pattern)
    }
  }

}
