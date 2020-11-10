package Specs.selection

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}
import fsm.symbolic.sre.{SREUtils, SelectionStrategy}

@RunWith(classOf[JUnitRunner])
class SkipTillNext extends FlatSpec with LazyLogging {
  "Skip-till-next selection strategy " should " produce less matches than skip-till-any " in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("\n\n\t testPatterns\n\n")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      val anyFormula = SREUtils.applySelection(formula, SelectionStrategy.ANY)
      val nextFormula = SREUtils.applySelection(formula, SelectionStrategy.NEXT)
      logger.debug("Testing pattern/formula: " + p.toString + "/" + formula.toString)
      val anySnfa = SNFAUtils.buildSNFA(anyFormula)
      val nextSnfa = SNFAUtils.buildSNFA(nextFormula)
      var anySnfaMatches = 0
      var nextSnfaMatches = 0
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val anySnfaAccepts = anySnfa.accepts(events)
        val nextSnfaAccepts = nextSnfa.accepts(events)
        logger.debug("\t\t Result anySnfa/nextSnfa: " + anySnfaAccepts + "/" + nextSnfaAccepts)
        if (nextSnfaAccepts) {
          nextSnfaMatches += 1
          assert(anySnfaAccepts)
        }
        if (anySnfaAccepts) anySnfaMatches += 1
      }
      logger.debug("Matches for skip-till-next: " + nextSnfaMatches)
      logger.debug("Matches for skip-till-any: " + anySnfaMatches)
    }
  }

}
