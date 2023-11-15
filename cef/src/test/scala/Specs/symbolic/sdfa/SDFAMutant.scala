package Specs.symbolic.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sre.SREUtils
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAUtils}
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

  "Building DQ with graph SDFA" should "should be faster than standard SDFA" in {
    testDQMulti()
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

  def testDQMulti(): Unit = {
    logger.debug("\n\n\t testDQMulti\n\n")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    val formulas = patterns.map(p => SREUtils.re2formula(p))
    val snfas = formulas.map(f => SNFAUtils.buildSNFA(f))
    val sdfas = snfas.map(snfa => SFAUtils.determinizeI(snfa, Set(exclusive), Set.empty))
    val sdfasit = sdfas.iterator
    var tmTotal: Long = 0
    var tgTotal: Long = 0
    while (sdfasit.hasNext) {
      val sdfa = sdfasit.next()
      val (tm, tg): (Long, Long) = testDQ(sdfa)
      tmTotal += tm
      tgTotal += tg
    }
    logger.debug("Total time for Mutant/Graph: " + (tmTotal / 1000.0) + "/" + (tgTotal / 1000.0))
  }

  def testDQ(sdfa: SDFA): (Long, Long) = {
    val sdfam = SDFAUtils.mutate(sdfa)
    val sdfag = SDFAUtils.mutate2graph(sdfa)

    logger.debug("Testing DQ for sdfa: " + sdfa.toString)

    var tm: Long = 0
    var tg: Long = 0
    val maxM = 2
    val sentences = sdfa.sentences
    val Ams = utils.SetUtils.permutationsAlt(sentences, maxM)
    for (m <- 1 to maxM) {
      logger.debug("Testing for m=" + m)
      val qsit = sdfa.states.keySet.iterator
      while (qsit.hasNext) {
        val q = qsit.next()
        val tm1 = System.nanoTime()
        val dqm = sdfam.buildDQ(q, Ams(m))
        val tm2 = System.nanoTime()
        tm += (tm2 - tm1)
        logger.debug("DQ Mutant from " + q + " for m=" + m + ":" + dqm)
        val tg1 = System.nanoTime()
        val dqg = sdfag.getDQ(q, m)
        val tg2 = System.nanoTime()
        tg += (tg2 - tg1)
        logger.debug("Graph DQ from " + q + " for m=" + m + ":" + dqg)
        assert(dqm == dqg)
      }
    }
    logger.debug("Time for Mutant/Graph: " + (tm / 1000.0) + "/" + (tg / 1000.0))
    assert(tg <= tm)
    (tm, tg)
  }



}
