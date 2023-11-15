package Specs.symbolic.snfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SNFAeqSDFA extends FlatSpec with LazyLogging {
  "SNFA" should "accept same words as corresponding SDFA" in {
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
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SNFA-SDFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfa1 = SFAUtils.determinize(snfa)
      val sdfa2 = SFAUtils.determinize(snfa, Set(exclusive), Set.empty)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sdfa1Accepts = sdfa1.accepts(events)
        val sdfa2Accepts = sdfa2.accepts(events)
        logger.debug("\t\t Result snfa/sdfa1/sdfa2: " +
          snfaAccepts + "/" + sdfa1Accepts + "/" + sdfa2Accepts)
        assert(snfaAccepts == sdfa1Accepts)
        assert(sdfa1Accepts == sdfa2Accepts)
      }
    }
  }

}
