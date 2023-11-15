package Specs.symbolic.sdfa

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
class SDFAIncrDet extends FlatSpec with LazyLogging {
  "Incremental determinization " should " produce SDFA that are equivalent to SDFA with standard determinization" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C", "D"), ConfigUtils.patternMaxDepth)
    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C", "D"), ConfigUtils.wordMaxLength)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val pd = PredicateConstructor.getEventTypePred("D")
    val exclusive = Set(pa, pb, pc, pd)
    var td: Long = 0
    var tdi: Long = 0
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SNFA-SDFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val t1 = System.nanoTime()
      val sdfa = SFAUtils.determinize(snfa, Set(exclusive), Set.empty)
      val t2 = System.nanoTime()
      val sdfai = SFAUtils.determinizeI(snfa, Set(exclusive), Set.empty)
      val t3 = System.nanoTime()
      td += (t2 - t1)
      tdi += (t3 - t2)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sdfaAccepts = sdfa.accepts(events)
        val sdfaiAccepts = sdfai.accepts(events)
        logger.debug("\t\t Result snfa/sdfa/sdfai: " +
          snfaAccepts + "/" + sdfaAccepts + "/" + sdfaiAccepts)
        assert(snfaAccepts == sdfaAccepts)
        assert(sdfaAccepts == sdfaiAccepts)
      }
    }
    logger.debug("Powerset determinization time: " + td / 1000000 + " ms.")
    logger.debug("Incremental determinization time: " + tdi / 1000000 + " ms.")
  }

}
