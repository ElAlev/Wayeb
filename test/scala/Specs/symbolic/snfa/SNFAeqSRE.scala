package Specs.symbolic.snfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.{LogicAtomicSentence, LogicConstant, LogicPredicate, RegularOperator, SREOperator, SRESentence, SREUtils}
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SNFAeqSRE extends FlatSpec with LazyLogging {
  "SNFA" should "accept same words as corresponding SRE" in {
    simpleTest()
    testPatterns()
  }

  def simpleTest(): Unit = {
    logger.debug("simple test")
    val pa = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("A"))))
    val pb = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("B"))))
    val itera = SREOperator(RegularOperator.ITER, List(pa))
    val iteraseqb = SREOperator(RegularOperator.SEQ, List(itera, pb))
    val word = List("A", "A", "B")
    val events = word.map(c => GenericEvent(c, 0))
    val formula = iteraseqb
    val formulaAccepts = formula.accepts(events)
    val snfa = SNFAUtils.buildSNFA(formula)
    val snfaAccepts = snfa.accepts(events)
    assert(formulaAccepts == snfaAccepts)
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SNFA-SRE equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sreAccepts = formula.accepts(events)
        logger.debug("\t\t Result snfa/sre: " + snfaAccepts + "/" + sreAccepts)
        assert(snfaAccepts == sreAccepts)
      }
    }
  }
}
