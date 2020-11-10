package Specs.symbolic.snfa

import com.typesafe.scalalogging.LazyLogging
import fsm.classical.fa.nfa.NFAFactory
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class NFAEqSNFA extends FlatSpec with LazyLogging {
  "SNFA" should "accept same words as corresponding NFA" in {
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
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing NFA-SNFA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val elsnfa = SNFAUtils.eliminateEpsilon(snfa)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val nfa = NFAFactory.buildNFA(p, word.toSet)
        val snfaAccepts = snfa.accepts(events)
        val elsnfaAccepts = elsnfa.accepts(events)
        val nfaAccepts = nfa.accepts(word)
        logger.debug("\t\t Result snfa/nfa: " + snfaAccepts + "/" + nfaAccepts)
        assert(snfaAccepts == nfaAccepts)
        assert(!elsnfa.hasEpsilon)
        logger.debug("\t\t Result snfa/elsnfa: " + snfaAccepts + "/" + elsnfaAccepts)
        assert(snfaAccepts == elsnfaAccepts)
      }
    }
  }

}
