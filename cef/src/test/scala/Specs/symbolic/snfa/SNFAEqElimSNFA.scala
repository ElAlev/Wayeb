package Specs.symbolic.snfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre.{LogicAtomicSentence, LogicConstant, LogicPredicate, RegularOperator, SREOperator, SRESentence, SREUtils}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class SNFAEqElimSNFA extends FlatSpec with LazyLogging {
  "SNFA" should "accept same words as corresponding SNFA with no epsilon transitions" in {
    testPatterns()
    simpleTest()
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
      logger.debug("Testing SNFA-no epsilon SNFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val elsnfa = SNFAUtils.eliminateEpsilon(snfa)
      logger.debug("\n\n\t\tSNFA:\n" + snfa.toString)
      logger.debug("\n\n\t\tno epsilon SNFA:\n" + elsnfa.toString)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val snfaAccepts = snfa.accepts(events)
        val elsnfaAccepts = elsnfa.accepts(events)
        logger.debug("\t\t Result snfa/elsnfa: " +
          snfaAccepts + "/" + elsnfaAccepts)
        assert(snfaAccepts == elsnfaAccepts)
      }
    }
  }

  def simpleTest(): Unit = {
    logger.debug("simple test")
    val pa = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("A"))))
    val pb = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("B"))))
    val seqab = SREOperator(RegularOperator.SEQ, List(pa, pb))
    val union_seqab_a = SREOperator(RegularOperator.CHOICE, List(seqab, pa))
    val word = List("A", "B")
    val events = word.map(c => GenericEvent(c, 0))
    val formula = union_seqab_a
    val formulaAccepts = formula.accepts(events)
    val snfa = SNFAUtils.buildSNFA(formula)
    val elsnfa = SNFAUtils.eliminateEpsilon(snfa)
    val snfaAccepts = snfa.accepts(events)
    val elsnfaAccepts = elsnfa.accepts(events)
    logger.debug("\t\t Result snfa/elsnfa: " +
      snfaAccepts + "/" + elsnfaAccepts)
    assert(formulaAccepts == snfaAccepts)
    assert(snfaAccepts == elsnfaAccepts)
  }

}
