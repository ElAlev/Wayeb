package Specs.selection

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class TransformToStrict extends FlatSpec with LazyLogging {
  "Applying strict contiguity to a formula " should " have no effect " in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      val strictFormula = SREUtils.applySelection(formula, SelectionStrategy.STRICT)
      logger.debug("Testing pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val strictSnfa = SNFAUtils.buildSNFA(strictFormula)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val strictSnfaAccepts = strictSnfa.accepts(events)
        logger.debug("\t\t Result snfa/strictSnfa: " + snfaAccepts + "/" + strictSnfaAccepts)
        assert(snfaAccepts == strictSnfaAccepts)
      }
    }
  }
}
