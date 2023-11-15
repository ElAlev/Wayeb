package Specs.symbolic.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.sdfa.SDFAUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SDFADis extends FlatSpec with LazyLogging {

  "SDFA" should "accept same words as corresponding disambiguated SDFA" in {
    val maxM = 2
    (1 to maxM).toList.foreach(m => testPatterns(m))
  }

  def testPatterns(order: Int): Unit = {
    logger.debug("testPatterns @ order " + order)
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SNFA-SDFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfa1 = SFAUtils.determinizeI(snfa)
      val sdfa2 = SFAUtils.determinizeI(snfa, Set(exclusive), Set.empty)
      val sdfa2dis = SDFAUtils.disambiguateMutant(sdfa2, order)
      val sdfa1dis = SDFAUtils.disambiguateMutant(sdfa1, order)
      assert(SDFAUtils.isMUnambiguous(sdfa1dis, order))
      assert(SDFAUtils.isMUnambiguous(sdfa2dis, order))
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sdfa1Accepts = sdfa1.accepts(events)
        val sdfa2Accepts = sdfa2.accepts(events)
        val sdfa1disAccepts = sdfa1dis.accepts(events)
        val sdfa2disAccepts = sdfa2dis.accepts(events)
        logger.debug("\t\t Result snfa/sdfa1/sdfa2/sdfa1/sdfa2: " +
          snfaAccepts + "/" + sdfa1Accepts + "/" + sdfa2Accepts + "/" + sdfa1disAccepts + "/" + sdfa2disAccepts)
        assert(snfaAccepts == sdfa1Accepts)
        assert(sdfa1Accepts == sdfa2Accepts)
        assert(sdfa2Accepts == sdfa1disAccepts)
        assert(sdfa1disAccepts == sdfa2disAccepts)
        assert(snfaAccepts == sdfa2disAccepts)
      }
    }
  }
}
