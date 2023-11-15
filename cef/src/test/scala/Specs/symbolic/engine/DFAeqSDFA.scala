package Specs.symbolic.engine

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.SREUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import fsm.CountPolicy.{NONOVERLAP, OVERLAP}
import fsm.symbolic.logic.PredicateConstructor
import stream.StreamFactory
import stream.source.EmitMode
import ui.ConfigUtils
import utils.testing.PatternGenerator
import workflow.provider.source.dfa.DFASourceRegExp
import workflow.provider.source.sdfa.SDFASourceFormula
import workflow.provider.{DFAProvider, FSMProvider, SDFAProvider}
import workflow.task.engineTask.ERFTask

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class DFAeqSDFA extends FlatSpec with LazyLogging {
  "DFA" should "find same occurrences as SDFA" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val policies = Set(OVERLAP, NONOVERLAP)
    val order = ConfigUtils.defaultOrder
    val partitionAttr = ConfigUtils.singlePartitionVal
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val ss = StreamFactory.getStreamSource(100, mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5), 10)
    val es = ss.emitEventsAndClose(EmitMode.BUFFER)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(Set(pa, pb, pc))
    for (pattern <- patterns) {
      val formula = SREUtils.re2formula(pattern)
      for (policy <- policies) {
        logger.debug("Testing DFA-SDFA equivalence for pattern/formula: " +
          pattern.toString + "/" + formula.toString + "\n" +
          "with policy: " + policy)

        val dfaProvider = DFAProvider(DFASourceRegExp(pattern, policy, order, es.getEventTypes))
        val fsmp1 = FSMProvider(dfaProvider)
        val erf1 = ERFTask(fsmp1, ss)

        val sdfaProvider = SDFAProvider(
          SDFASourceFormula(List((formula, order, partitionAttr, -1, "count")), policy, exclusive, Set.empty)
        )
        val fsmp2 = FSMProvider(sdfaProvider)
        val erf2 = ERFTask(fsmp2, ss)

        val profiler1 = erf1.execute()
        val profiler2 = erf2.execute()

        val compare = profiler1.getMatchDump.checkAgainst(profiler2.getMatchDump)
        if (!compare) {
          logger.debug("\t\t\nSTREAM\n\n" + es.getAsString)
          logger.debug("\t\t\nDFA\n\n" + erf1.getEngine.getFSMs.head.toString)
          logger.debug("\t\t\nMATCH DUMP 1\n\n" + profiler1.getMatchDump.toString)
          logger.debug("\t\t\nSDFA\n\n" + erf2.getEngine.getFSMs.head.toString)
          logger.debug("\t\t\nMATCH DUMP 2\n\n" + profiler2.getMatchDump.toString)
        }
        assert(compare)
      }
    }
  }
}
