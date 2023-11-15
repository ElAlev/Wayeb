package Specs.srem

import com.typesafe.scalalogging.LazyLogging
import fsm.{DSRAInterface, NSRAInterface}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import profiler.WtProfiler
import stream.array.EventStream
import stream.source.{ArrayStreamSource, EmitMode, StreamSource}
import stream.{GenericEvent, StreamFactory}
import ui.ConfigUtils
import utils.testing.PatternGenerator
import workflow.provider.source.dsra.{DSRASourceFromSREM, DSRASourceRegExp}
import workflow.provider.source.nsra.{NSRASourceFromSREM, NSRASourceRegExp}
import workflow.provider.{DSRAProvider, FSMProvider, NSRAProvider}
import workflow.task.engineTask.ERFTask

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class NSRAeqDSRA extends FlatSpec with LazyLogging {
  "Engine running with NSRA" should "should produce matches at the same points as engine running with DSRA" in {
    testPatterns()
    testPattern()
  }

  def testPattern(): Unit = {
    val home = System.getenv("WAYEB_HOME")
    //val ss = StreamFactory.getStreamSource(100, mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5), 10, Set(1,2))

    val es = EventStream(ArrayBuffer(
      GenericEvent(95, "A", 95, Map("attr" -> 2)),
      GenericEvent(96, "C", 96, Map("attr" -> 2)),
      GenericEvent(97, "B", 97, Map("attr" -> 1)),
      GenericEvent(98, "A", 98, Map("attr" -> 2)),
      GenericEvent(99, "B", 99, Map("attr" -> 2)),
      GenericEvent(100, "B", 100, Map("attr" -> 2))))
    val ss = ArrayStreamSource(es)

    val patternFile = home + "/patterns/validation/pattern2.sre"
    val dsrap = DSRAProvider(DSRASourceFromSREM(patternFile, ""))
    val fsmpDSRA = FSMProvider(dsrap)
    val erfDSRA = ERFTask(fsmpDSRA, ss)
    val profDSRA = erfDSRA.execute()

    val nsrap = NSRAProvider(NSRASourceFromSREM(patternFile))
    val fsmpNSRA = FSMProvider(nsrap)
    val erfNSRA = ERFTask(fsmpNSRA, ss)
    val profNSRA = erfNSRA.execute()

    assert(compare(profDSRA, profNSRA, erfDSRA, erfNSRA, ss))
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfWPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth,
      Set("x", "y")
    )

    val ss = StreamFactory.getStreamSource(100, mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5), 10, Set(1,2))

    for (pattern <- patterns) {
      logger.debug("\n\nTesting NSRA-DSRA engines  for pattern/formula: " + pattern.toString)

      logger.debug("\n\nRunning with NSRA\n\n")
      val nsrap = NSRAProvider(NSRASourceRegExp(pattern, "$", 3))
      val fsmpNSRA = FSMProvider(nsrap)
      val erfNSRA = ERFTask(fsmpNSRA, ss)
      val profNSRA = erfNSRA.execute()

      logger.debug("\n\nRunning with DSRA\n\n")
      val dsrap = DSRAProvider(DSRASourceRegExp(pattern, "$", 3))
      val fsmpDSRA = FSMProvider(dsrap)
      val erfDSRA = ERFTask(fsmpDSRA, ss)
      val profDSRA = erfDSRA.execute()

      assert(compare(profDSRA, profNSRA, erfDSRA, erfNSRA, ss))
    }
  }

  private def compare(
                       profDSRA: WtProfiler,
                       profNSRA: WtProfiler,
                       erfDSRA: ERFTask,
                       erfNSRA: ERFTask,
                       ss: StreamSource
                     ): Boolean = {
    val compare = profDSRA.getMatchDump.checkAgainstNonDet(profNSRA.getMatchDump)
    if (!compare._1) {
      val es = ss.emitEventsAndClose(EmitMode.BUFFER)
      logger.debug("\t\t\nSTREAM\n\n" + es.getAsString)
      val nsra = erfNSRA.getEngine.getFSMs.head.asInstanceOf[NSRAInterface].nsra
      logger.debug("\t\t\nNSRA\n\n" + nsra.toString)
      logger.debug("\t\t\nMATCH DUMP FROM NSRA\n\n" + profNSRA.getMatchDump.toStringWithConfs)
      val dsra = erfDSRA.getEngine.getFSMs.head.asInstanceOf[DSRAInterface].dsra
      logger.debug("\t\t\nDSRA\n\n" + dsra.toString)
      logger.debug("\t\t\nMATCH DUMP FROM DSRA\n\n" + profDSRA.getMatchDump.toStringWithConfs)
      logger.debug("\t\t\nORPHANS FROM NSRA\n\n" + compare._3)
      logger.debug("\t\t\nORPHANS FROM DSRA\n\n" + compare._2)
    }
    compare._1
  }


}
