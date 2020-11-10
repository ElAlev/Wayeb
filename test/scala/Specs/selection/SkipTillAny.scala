package Specs.selection

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}
import fsm.symbolic.sre.{
  LogicAtomicSentence,
  LogicConstant,
  LogicPredicate,
  RegularOperator,
  SREOperator,
  SRESentence,
  SREUtils,
  SelectionStrategy
}

@RunWith(classOf[JUnitRunner])
class SkipTillAny extends FlatSpec with LazyLogging {
  "Skip-till-any selection strategy " should " produce more matches than strict-contiguity " in {
    simpleTest()
    testPatterns()
    complicatedTestForIteration()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      val anyFormula = SREUtils.applySelection(formula, SelectionStrategy.ANY)
      logger.debug("Testing pattern/formula: " + p.toString + " " + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val anySnfa = SNFAUtils.buildSNFA(anyFormula)
      var snfaMatches = 0
      var anySnfaMatches = 0
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val anySnfaAccepts = anySnfa.accepts(events)
        logger.debug("\t\t Result snfa/anySnfa: " + snfaAccepts + "/" + anySnfaAccepts)
        if (snfaAccepts) {
          snfaMatches += 1
          assert(anySnfaAccepts)
        }
        if (anySnfaAccepts) anySnfaMatches += 1
      }
      logger.debug("Matches for strict contiguity: " + snfaMatches)
      logger.debug("Matches for skip-till-any: " + anySnfaMatches)
    }
  }

  def simpleTest(): Unit = {
    logger.debug("simple test for iteration")
    val pa = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("A"))))
    val itera = SREOperator(RegularOperator.ITER, List(pa))
    val words = List(List("A", "B", "A"))
    val formula = itera
    val anyFormula = SREUtils.applySelection(itera, SelectionStrategy.ANY)
    val snfa = SNFAUtils.buildSNFA(formula)
    val anySnfa = SNFAUtils.buildSNFA(anyFormula)
    for (word <- words) {
      val events = word.map(c => GenericEvent(c, 0))
      val snfaAccepts = snfa.accepts(events)
      val anySnfaAccepts = anySnfa.accepts(events)
      logger.debug("\t\t Result snfa/anySnfa for word " + word + ":/" + snfaAccepts + "/" + anySnfaAccepts)
      assert(snfaAccepts != anySnfaAccepts)
    }
  }

  /**
    * pattern = (A;B)*
    * sequence = "A", "B", "C", "A", "B"
    *
    * strict should not accept because of C in the middle
    * any should skip C and accept
    */
  def complicatedTestForIteration(): Unit = {
    logger.debug("complicated test for iteration\n\n")
    val pa = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("A"))))
    val pb = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("B"))))
    val aseqb = SREOperator(RegularOperator.SEQ, List(pa, pb))
    val formula = SREOperator(RegularOperator.ITER, List(aseqb))
    val word = List("A", "B", "C", "A", "B")
    val events = word.map(c => GenericEvent(c, 0))
    val anyFormula = SREUtils.applySelection(formula, SelectionStrategy.ANY)
    val snfa = SNFAUtils.buildSNFA(formula)
    val anySnfa = SNFAUtils.buildSNFA(anyFormula)
    val snfaAccepts = snfa.accepts(events)
    val anySnfaAccepts = anySnfa.accepts(events)
    logger.debug("\t\t Result snfa/anySnfa: " + snfaAccepts + "/" + anySnfaAccepts)
    assert(snfaAccepts != anySnfaAccepts)
  }

}
