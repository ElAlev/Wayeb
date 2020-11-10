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
class SDFAMutantDis extends FlatSpec with LazyLogging {

  "SDFAMutant" should "should be equivalent to SDFA" in {
    testPatterns(1)
    testPatterns(2)
  }

  def testPatterns(m: Int): Unit = {
    logger.debug("test patterns @ " + m)
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing SDFA-SDFA mutant equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfa = SFAUtils.determinize(snfa, Set(exclusive), Set.empty)
      val sdfadis = SDFAUtils.disambiguateImmut(sdfa, m)
      val sdfadismut = SDFAUtils.disambiguateMutant(sdfa, m)

      logger.debug("SDFADis @" + m + "\n" + sdfadis.toString)
      logger.debug("SDFADisMut @" + m + "\n" + sdfadismut.toString)
      assert(SDFAUtils.isMUnambiguous(sdfadis, m))
      assert(SDFAUtils.isMUnambiguous(sdfadismut, m))
      assert(sdfadis.states.size == sdfadismut.states.size)
      assert(sdfadis.states == sdfadismut.states)
      assert(sdfadis.transitions.size == sdfadismut.transitions.size)
      assert(sdfadis.start == sdfadismut.start)
      assert(sdfadis.finals == sdfadismut.finals)
      assert(sdfadis.duplicates == sdfadismut.duplicates)

      val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
      logger.debug("Testing SDFAMutant equivalence for formula: " + formula.toString)
      for (word <- words) {
        print("...with word " + word)
        val events = word.toList.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val sdfaAccepts = sdfa.accepts(events)
        val sdfaDisAccepts = sdfadis.accepts(events)
        val sdfaDismutAccepts = sdfadismut.accepts(events)
        logger.debug("\t\t Result snfa/sdfa1/sdfa1dis/sdfa1dismut: " +
          snfaAccepts + "/" + sdfaAccepts + "/" + sdfaDisAccepts + "/" + sdfaDismutAccepts)
        assert(snfaAccepts == sdfaAccepts)
        assert(sdfaAccepts == sdfaDisAccepts)
        assert(sdfaDisAccepts == sdfaDismutAccepts)
      }
    }
  }

}
