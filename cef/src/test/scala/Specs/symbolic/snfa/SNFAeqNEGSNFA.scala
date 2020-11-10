package Specs.symbolic.snfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre.{SREOperator, SREUtils}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SNFAeqNEGSNFA extends FlatSpec with LazyLogging {

  "SNFA " should " accept the complement of words accepted by negated SNFA " in {
    testPatterns()
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
      val negFormula = SREOperator(fsm.symbolic.sre.RegularOperator.NEG, List(formula))
      logger.debug("Testing with...")
      logger.debug("...original formula: " + formula)
      logger.debug("...negated formula: " + negFormula)
      val snfa = SNFAUtils.buildSNFA(formula)
      val negSnfa = SNFAUtils.buildSNFA(negFormula)
      for (w <- words) {
        val event = w.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(event)
        val negSnfaAccepts = negSnfa.accepts(event)
        logger.debug("word " + w)
        logger.debug("SNFA:" + snfaAccepts)
        logger.debug("Negated SNFA" + negSnfaAccepts)
        assert(snfaAccepts == !negSnfaAccepts)
      }
    }
  }
}
