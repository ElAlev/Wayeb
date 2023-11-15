package utils.testing

import breeze.stats.distributions._
import fsm.classical.pattern.regexp.NodeType._
import fsm.classical.pattern.regexp.OperatorType._
import fsm.classical.pattern.regexp.{OperatorNode, RegExpTree, RegExpUtils, SymbolNode}
import utils.MathUtils.sampleUniform

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
    generateRegExpPatterns(total, symbols, maxDepth, Set.empty)
  }

  /**
    * Randomly generates total number of regular expression trees from a given set of symbols.
    * The maximum depth of each tree is maxDepth.
    *
    * @param total The total number of regular expression trees to be generated.
    * @param symbols The set of symbols.
    * @param maxDepth The maximum depth of each tree.
    * @param variables The variables to be used for register expressions and automata. If empty, assumes no registers.
    * @return The set of regular expression trees.
    */
  def generateRegExpPatterns(
                              total: Int,
                              symbols: Set[String],
                              maxDepth: Int,
                              variables: Set[String]
                            ): Set[RegExpTree] = {
    val patterns = mutable.Set[RegExpTree]()
    var patternsProduced = 0
    while (patternsProduced < total) {
      val newPattern = genRegExpPattern(0, maxDepth, symbols.toList, NONE, variables.toList)
      if (RegExpUtils.leastSymbolsNo(newPattern) != 0 & registerAccessValid(newPattern)) {
        patterns.add(newPattern)
        patternsProduced += 1
      }
    }
    patterns.toSet
  }

  private def registerAccessValid(rep: RegExpTree): Boolean = registerAccessValidAux(rep, Set.empty)

  private def registerAccessValidAux(
                                      rep: RegExpTree, definedVariables: Set[String]
                                    ): Boolean = {
    rep match {
      case SymbolNode(s, _) => {
        val split = s.split(",")
        val pred = split(0)
        if (pred.equalsIgnoreCase("EQAttr")) {
          val accessedVar = split(1)
          definedVariables.contains(accessedVar)
        }
        else true
      }
      case OperatorNode(ITER, List(x)) => registerAccessValidAux(x, definedVariables)
      case OperatorNode(UNION, List(x, y)) => {
        registerAccessValidAux(x, definedVariables) & registerAccessValidAux(y, definedVariables)
      }
      case OperatorNode(CONCAT, List(x, y)) => {
        val xvariables = getDefinedVariables(x)
        val allVariables = xvariables ++ definedVariables
        registerAccessValidAux(x, definedVariables) & registerAccessValidAux(y, allVariables)
      }
    }
  }

  private def getDefinedVariables(rep: RegExpTree): Set[String] = {
    rep match {
      case SymbolNode(_, w) => {
        w match {
          case Some(x) => Set(x)
          case None => Set.empty
        }
      }
      // iteration means zero or more, worst case is zero, thus no vars
      case OperatorNode(ITER, List(x)) => Set.empty
      // to ensure that a var accessed after a union is valid, we must ensure that it has been defined in all previous
      // paths, thus we take the intersection
      case OperatorNode(UNION, List(x, y)) => getDefinedVariables(x) & getDefinedVariables(y)
      case OperatorNode(CONCAT, List(x, y)) => getDefinedVariables(x) ++ getDefinedVariables(y)
    }
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
                                previousOp: OperatorType,
                                variables: List[String]
                              ): RegExpTree = {
    if (currentDepth == maxDepth) sampleTerminalExpression(symbols, variables) //SymbolNode(sampleString(symbols))
    else {
      val nodeType = sampleNodeType()
      nodeType match {
        case SYMBOL => sampleTerminalExpression(symbols, variables) //SymbolNode(sampleString(symbols))
        case OPERATOR => {
          val op = sampleOperator(previousOp)
          op match {
            case ITER => OperatorNode(ITER, List(genRegExpPattern(currentDepth + 1, maxDepth, symbols, op, variables)))
            case _ => OperatorNode(
              op,
              List(
                genRegExpPattern(currentDepth + 1, maxDepth, symbols, op, variables),
                genRegExpPattern(currentDepth + 1, maxDepth, symbols, op, variables)
              )
            )
          }
        }
      }
    }
  }

  private def sampleTerminalExpression(
                                        symbols: List[String],
                                        variables: List[String]
                                      ): SymbolNode = {
    val isSymbol = selectSymbolOrRegister(variables)
    val predicate = if (isSymbol) sampleString(symbols) else "EQAttr," + sampleString(variables)
    if (variables.isEmpty) {
      SymbolNode(predicate)
    }
    else {
      if (sampleUniform(1) < 0.5) SymbolNode(predicate, Option(sampleString(variables)))
      else SymbolNode(predicate)
    }
  }

  private def selectSymbolOrRegister(variables: List[String]): Boolean = {
    if (variables.isEmpty) true
    else {
      val s = sampleUniform(1)
      (s <= 0.5)
    }
  }

  /**
    * Randomly picks a symbol for a given set.
    *
    * @param symbols The given set of symbols.
    * @return The random symbol chosen.
    */
  private def sampleString(symbols: List[String]): String = {
    val s = sampleUniform(symbols.size).toInt
    symbols(s)
  }

  /**
    * @return A random choice between symbol and operator.
    */
  private def sampleNodeType(): NodeType = {
    val s = sampleUniform(1)
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
    val s = sampleUniform(3)
    if (s <= 1.0) CONCAT
    else if (s <= 2.0) UNION
    else ITER
  }

}
