package fsm.symbolic.sre

import java.io.FileReader
import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.IdGenerator
import fsm.symbolic.sfa.Constants.truePredicate
import fsm.symbolic.sre.SelectionStrategy.SelectionStrategy
import fsm.classical.pattern.regexp.{OperatorNode, RegExpTree, SymbolNode}
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sre.SelectionUtils.{transformFormulaNoSelection, transformFormulaWithSelection}

object SREUtils extends SREParser with DeclarationsParser with LazyLogging {

  /**
    * Compiles a file with patterns into a list of formulas.
    *
    * @param fn The file with the patterns
    * @return a list of Tuple3 where each Tuple3 contains the formula, its order and its partition attribute.
    */
  def parseSRE(fn: String): List[(SREFormula, Int, String, Int, String)] = {
    val reader = new FileReader(fn)
    val parsed = parseAll(formulasList, reader)
    val f = parsed.get
    f
  }

  /**
    * Parses declarations file with exclusives and extras.
    *
    * @param fn The path to the declarations file.
    * @return a Tuple2 with a list of exclusives and a list of extras.
    */
  def parseDeclarations(fn: String): (List[Exclusive], List[Extras]) = {
    val reader = new FileReader(fn)
    val parsed = parseAll(declarations, reader)
    val f = parsed.get
    partitionDeclarations(f, List.empty[Exclusive], List.empty[Extras])
  }

  /**
    * Gathers all exclusives together in a list and all extras together in another list. May be mixed up in declarations
    * file.
    *
    * @param dec The initial list of declarations as parsed.
    * @param exclusive Accumulator for exclusives (recursion).
    * @param extras Accumulator for extras (recursion).
    * @return a Tuple2 with a list of exclusives and another of extras.
    */
  @scala.annotation.tailrec
  private def partitionDeclarations(
                                     dec: List[Declaration],
                                     exclusive: List[Exclusive],
                                     extras: List[Extras]
                                   ): (List[Exclusive], List[Extras]) = {
    dec match {
      case Nil => (exclusive, extras)
      case head :: tail => {
        head match {
          case x: Exclusive => partitionDeclarations(tail, x :: exclusive, extras)
          case x: Extras => partitionDeclarations(tail, exclusive, x :: extras)
        }
      }
    }
  }

  /**
    * Parse a file of patterns and a file of declarations and return a list of formulas and sets of exclusives and
    * extras. SREUtils.parseSRE returns a list Tuple3 where each Tuple3 contains the formula, the order and the
    * partition attribute.
    *
    * See fsm.symbolic.sre.SREParser and fsm.symbolic.sre.DeclarationsParser for more explanations about formulas
    * and declarations.
    *
    * @param patternsFile The file containing the patterns.
    * @param declarationsFile The file containing the declarations, i.e., extras and/or exclusives.
    * @param withSelection Boolean to determine whether the formula will be treated according to selection strategies
    * @return a Tuple3 x where x._1 a list of formulas, x._2 a set of exclusives and x._3 a set of extra sentences
    */
  def sre2formulas(
                    patternsFile: String,
                    declarationsFile: String,
                    withSelection: Boolean
                  ): (List[(SREFormula, Int, String, Int, String)], Set[Set[Predicate]], Set[Sentence]) = {
    logger.info("Parsing formulas from " + patternsFile)
    val formulas = SREUtils.parseSRE(patternsFile)
    logger.debug("Parsed formulas: " + formulas.toString)
    val (exclusives, extras) = if (declarationsFile == "") (Set.empty[Set[Predicate]], Set.empty[Sentence]) else {
      logger.info("Parsing declarations from " + declarationsFile)
      val d = SREUtils.parseDeclarations(declarationsFile)
      val exc = d._1.map(e => e.getPredicates).toSet
      val ext = d._2.flatMap(e => e.getSentences).toSet
      logger.debug("Parsed declarations: " + exc.toString())
      (exc, ext)
    }
    logger.debug("Checking for selection strategies")
    val transformedFormulas =
      if (withSelection) formulas.map(f => (transformFormulaWithSelection(f._1), f._2, f._3, f._4, f._5))
      else formulas.map(f => (transformFormulaNoSelection(f._1), f._2, f._3, f._4, f._5))
    (transformedFormulas, exclusives, extras)
  }

  /**
    * Takes a formula and a selection strategy as inputs and creates a formula re-written so that it respects
    * the selection strategy.
    *
    * @param f the original formula
    * @param selectionStrategy The selection strategy
    * @return The re-written formula
    */
  def applySelection(
                      f: SREFormula,
                      selectionStrategy: SelectionStrategy
                    ): SREFormula = {
    val selectionFormula = selectionStrategy match {
      case SelectionStrategy.STRICT => f // STRICT is simply ignored (assumed to be the default strategy).
      case SelectionStrategy.ANY => SREOperator(RegularOperator.ANY, List(f))
      case SelectionStrategy.NEXT => SREOperator(RegularOperator.NEXT, List(f))
    }
    val transformedFormula = transformFormulaWithSelection(selectionFormula)
    transformedFormula
  }

  /**
    * Converts a regular expression tree to a formula. Used for testing.
    *
    * @param re A regular expression tree.
    * @return The SRE formula corresponding to the tree.
    */
  def re2formula(re: RegExpTree): SREFormula = {
    val idg = IdGenerator()
    re2formula(re, idg)
  }

  private def re2formula(
                          re: RegExpTree,
                          idg: IdGenerator
                        ): SREFormula = {
    re match {
      case SymbolNode(symbol, writeReg) => {
        val atomicSentence = if (symbol.length == 1) {
          LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant(symbol)))
        }
        else {
          val spl = symbol.split(",")
          val predicate  = LogicPredicate(spl(0))
          val logicVar = LogicVariable("attr")
          val regVar = RegisterVariable(spl(1))
          LogicAtomicSentence(predicate, List(logicVar,regVar))
        }
        val sentence = writeReg match {
          case Some(r) => SRESentence(atomicSentence, RegisterVariable(r))
          case None => SRESentence(atomicSentence)
        }
        sentence
      }
      case OperatorNode(fsm.classical.pattern.regexp.OperatorType.CONCAT, children) => {
        val leftSRE = re2formula(children.head, idg)
        val rightSRE = re2formula(children(1), idg)
        SREOperator(fsm.symbolic.sre.RegularOperator.SEQ, List(leftSRE, rightSRE))
      }
      case OperatorNode(fsm.classical.pattern.regexp.OperatorType.UNION, children) => {
        val leftSRE = re2formula(children.head, idg)
        val rightSRE = re2formula(children(1), idg)
        SREOperator(fsm.symbolic.sre.RegularOperator.CHOICE, List(leftSRE, rightSRE))
      }
      case OperatorNode(fsm.classical.pattern.regexp.OperatorType.ITER, children) => {
        val sre = re2formula(children.head, idg)
        SREOperator(fsm.symbolic.sre.RegularOperator.ITER, List(sre))
      }
      case _ => throw new IllegalArgumentException("Unknown node " + re.toString)
    }
  }

  def addSigmaStar(formula: SREFormula): SREFormula = {
    val trueSentence = SRESentence(LogicAtomicSentence(LogicPredicate(truePredicate), List.empty))
    val sigmaStar = SREOperator(fsm.symbolic.sre.RegularOperator.ITER, List(trueSentence))
    SREOperator(fsm.symbolic.sre.RegularOperator.SEQ, List(sigmaStar, formula))
  }

}
