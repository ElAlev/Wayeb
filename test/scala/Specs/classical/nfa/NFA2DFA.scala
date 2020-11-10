package Specs.classical.nfa

import com.typesafe.scalalogging.LazyLogging
import fsm.classical.fa.dfa.DFAUtils
import fsm.classical.fa.nfa.NFAFactory
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class NFA2DFA extends FlatSpec with LazyLogging {
  "epsilon-NFAs " should " be converted to equivalent DFAs and these DFAs should produce same results as AC DFAs " in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("Testing with patterns and words.")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("a", "b", "c"),
      ConfigUtils.patternMaxDepth
    )
    val words = SymbolWordGenerator.generateStrings(Set("a", "b", "c"), ConfigUtils.wordMaxLength)
    for (p <- patterns) {
      logger.debug("Testing epsilon-NFA to DFA (eq) for pattern: " + p.toString)
      for (word <- words) {
        logger.debug("...with word " + word)
        val nfap = NFAFactory.buildNFA(p, word.toSet)
        val elnfap = NFAFactory.buildEliminatedNFA(p, word.toSet)
        assert(elnfap.isDFAEquivalent)
        val dfap = DFAUtils.convertNfa2Dfa(elnfap)
        logger.debug("Pattern: " + p.toString + " Word: " + word + "/" + elnfap.accepts(word) + "/" + dfap.accepts(word))
        assert(nfap.accepts(word) == dfap.accepts(word))
        assert(elnfap.accepts(word) == dfap.accepts(word))
      }
    }
  }

}
