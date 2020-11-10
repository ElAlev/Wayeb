package Specs.symbolic

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.logic.PredicateConstructor
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class MinTerms extends FlatSpec with LazyLogging {
  "The two methods for creating min-terms " should " produce SDFA with the same behavior" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C", "D"),
      ConfigUtils.patternMaxDepth
    )
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C", "D"), ConfigUtils.wordMaxLength)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val pd = PredicateConstructor.getEventTypePred("D")
    val exclusives = Set(Set(pa, pb), Set(pc, pd))
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SDFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfaWithSat = SFAUtils.determinizeI(snfa, exclusives, Set.empty)
      val sdfaWithoutSat = SFAUtils.determinizeI(snfa, exclusives, Set.empty, "withoutsat")
      logger.debug("Size of SDFA built with satisfiability checking: " + sdfaWithSat.size)
      logger.debug("Size of SDFA built without satisfiability checking: " + sdfaWithoutSat.size)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sdfaWithSatAccepts = sdfaWithSat.accepts(events)
        val sdfaWithoutSatAccepts = sdfaWithoutSat.accepts(events)
        logger.debug("\t\t Result snfa/sdfaWithSat/sdfaWithoutSat: " +
          snfaAccepts + "/" + sdfaWithSatAccepts + "/" + sdfaWithoutSatAccepts)
        assert(snfaAccepts == sdfaWithSatAccepts)
        assert(sdfaWithSatAccepts == sdfaWithoutSatAccepts)
      }
    }
  }

}
