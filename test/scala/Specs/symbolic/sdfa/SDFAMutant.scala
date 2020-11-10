package Specs.symbolic.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.logic.PredicateConstructor
import fsm.symbolic.sfa.sdfa.SDFAUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SDFAMutant extends FlatSpec with LazyLogging {
  "SDFA with graph" should "should be equivalent to standard SDFA" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("\n\n\t testPatterns\n\n")
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
      logger.debug("Testing SDFA-SDFAG equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfa = SFAUtils.determinize(snfa, Set(exclusive), Set.empty)
      val sdfag = SDFAUtils.mutate2graph(sdfa)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val sdfaAccepts = sdfa.accepts(events)
        val sdfagAccepts = sdfag.accepts(events)
        logger.debug("\t\t Result sdfa/sdfag: " +
          sdfaAccepts + "/" + sdfagAccepts)
        assert(sdfaAccepts == sdfagAccepts)
      }
    }
  }

}
