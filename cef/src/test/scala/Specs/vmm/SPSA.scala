package Specs.vmm

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.{EpsilonSentence, LogicUtils, PredicateConstructor, Sentence}
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import model.vmm.mapper.Isomorphism
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import utils.testing.{PatternGenerator, SymbolWordGenerator}
import model.vmm.{Symbol, SymbolWord}
import model.vmm.pst.psa.PSAUtils
import model.vmm.pst.spsa.SPSAUtils
import model.vmm.pst.{CounterSuffixTree, CyclicBuffer, PSTLearner}
import ui.ConfigUtils

@RunWith(classOf[JUnitRunner])
class SPSA extends FlatSpec with LazyLogging {
  "SPSA" should " be language and probabilistically equivalent to SDFA " in {
    for (k <- 1 to ConfigUtils.maxOrder) testPatterns(k)
  }

  def testPatterns(k: Int): Unit = {
    require(k > 0)
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusives = Set(Set(pa, pb, pc))
    val extras = Set.empty[Sentence]

    val words = SymbolWordGenerator.generateStrings(Set("A", "B", "C"), ConfigUtils.wordMaxLength)

    for (pattern <- patterns) {
      val formula = SREUtils.re2formula(pattern)
      logger.debug("Testing SDFA-SPSA equivalence for pattern/formula: " + pattern.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sentences = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence] & !s.isTrue)
      val sentencesWithExtras = sentences ++ extras
      val minTerms = LogicUtils.buildMinTerms(sentencesWithExtras, exclusives)
      val iso = Isomorphism(minTerms)

      val stream = SymbolWordGenerator.generateSymbolStream(minTerms.size, ConfigUtils.symbolStreamSize)
      val buf = new CyclicBuffer(k + 1)
      val cst = CounterSuffixTree()
      var symbols: Set[Symbol] = Set.empty
      for (i <- stream.indices) {
        buf.pushSymbol(stream(i))
        val bufferedWord = buf.pop
        cst.updateWithNewWord(bufferedWord)
        symbols = symbols + stream(i)
      }
      val pstl = PSTLearner(symbols, k, ConfigUtils.maxNoStates)
      val pst = pstl.learnOriginal(cst, withMissing = true)
      val psta = pst.makePSACompatible()
      if (pst.getSize == 1) {
        logger.debug("PST of size 1 found")
      }
      val psa = PSAUtils.buildPSA(psta)

      val sdfa2 = SFAUtils.determinizeI(snfa, exclusives, extras)
      val spsa2 = SPSAUtils.buildSPSA(sdfa2, psa, iso)

      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(c => GenericEvent(c, 0))
        val sdfa2Accepts = sdfa2.accepts(events)
        val symbolWord = SymbolWord(events.map(e => iso.evaluate(e)))
        val spsa2Accepts = spsa2.accepts(symbolWord)
        logger.debug("SDFA accepts? " + sdfa2Accepts)
        logger.debug("SPSA accepts? " + spsa2Accepts)
        assert(sdfa2Accepts == spsa2Accepts)
        val canStart = spsa2.canStartWith(symbolWord)
        canStart match {
          case Some(x) => {
            for (symbol <- symbols) {
              val spsaprob = x._2.getProbFor(symbol)
              val pstprob = pst.getConditionalProbFor(symbol, symbolWord.word)
              logger.debug("SPSA prob, symbol=" + symbol + " context=" + symbolWord + " : " + spsaprob)
              logger.debug("PST prob, symbol=" + symbol + " context=" + symbolWord + " : " + pstprob)
              val spsaprobInt = (spsaprob * 1000).toInt
              val pstprobInt = (pstprob * 1000).toInt
              assert(spsaprobInt == pstprobInt)
            }
          }
          case None => logger.debug("Skipping " + symbolWord)
        }
      }
    }
  }

}
