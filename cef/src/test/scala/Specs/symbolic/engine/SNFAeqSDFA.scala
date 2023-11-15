package Specs.symbolic.engine

import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy
import fsm.symbolic.logic.PredicateConstructor
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.StreamFactory
import stream.source.EmitMode
import ui.ConfigUtils
import utils.testing.PatternGenerator
import workflow.provider.source.sdfa.SDFASourceRegExp
import workflow.provider.{FSMProvider, SDFAProvider, SNFAProvider}
import workflow.provider.source.snfa.SNFASourceRegExp
import workflow.task.engineTask.ERFTask

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class SNFAeqSDFA extends FlatSpec with LazyLogging {
  "Engine running with SNFA" should "should produce matches at the same points as engine running with SDFA" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val ss = StreamFactory.getStreamSource(100, mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5), 10)
    for (pattern <- patterns) {
      logger.debug("\n\nTesting SNFA-SDFA engines  for pattern/formula: " + pattern.toString)

      logger.debug("\n\nRunning with SNFA\n\n")
      val snfap = SNFAProvider(SNFASourceRegExp(pattern, "$", 0))
      val fsmpSNFA = FSMProvider(snfap)
      val erfSNFA = ERFTask(fsmpSNFA, ss)
      val profSNFA = erfSNFA.execute()

      logger.debug("\n\nRunning with SDFA\n\n")
      val pa = PredicateConstructor.getEventTypePred("A")
      val pb = PredicateConstructor.getEventTypePred("B")
      val pc = PredicateConstructor.getEventTypePred("C")
      val exclusive = Set(Set(pa, pb, pc))
      val sdfap = SDFAProvider(SDFASourceRegExp(pattern,0,"$",0,exclusives = exclusive, Set.empty, CountPolicy.OVERLAP))
      val fsmpSDFA = FSMProvider(sdfap)
      val erfSDFA = ERFTask(fsmpSDFA, ss)
      val profSDFA = erfSDFA.execute()

      val compare = profSDFA.getMatchDump.checkAgainstNonDet(profSNFA.getMatchDump)
      if (!compare._1) {
        val es = ss.emitEventsAndClose(EmitMode.BUFFER)
        logger.debug("\t\t\nSTREAM\n\n" + es.getAsString)
        logger.debug("\t\t\nSNFA\n\n" + erfSNFA.getEngine.getFSMs.head.toString)
        logger.debug("\t\t\nMATCH DUMP FROM SNFA\n\n" + profSNFA.getMatchDump.toString)
        logger.debug("\t\t\nSDFA\n\n" + erfSDFA.getEngine.getFSMs.head.toString)
        logger.debug("\t\t\nMATCH DUMP FROM SDFA\n\n" + profSDFA.getMatchDump.toString)
        logger.debug("\t\t\nORPHANS FROM SNFA\n\n" + compare._3)
        logger.debug("\t\t\nORPHANS FROM SDFA\n\n" + compare._2)
      }
      assert(compare._1)
    }
  }

}
