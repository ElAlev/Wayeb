package Specs.engine

import fsm.runtime.{Match, MatchDump}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import fsm.classical.pattern.regexp.RegExpUtils
import stream.StreamFactory
import stream.source.EmitMode
import ui.ConfigUtils
import workflow.provider.source.dfa.DFASourceRegExp
import workflow.provider.{DFAProvider, FSMProvider}
import workflow.task.engineTask.ERFTask
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class EngineSpec extends FlatSpec {
  "The pattern a;b in sequence a,b,a,b " should " produce two matches {1,2} and {3,4} " in {
    val mypattern = RegExpUtils.getConcatStr(List("a", "b"))
    val policy = ConfigUtils.defaultPolicy
    val ss = StreamFactory.getStreamSource(List("a", "b", "a", "b"))
    val es = ss.emitEventsAndClose(EmitMode.BUFFER)

    val md = new MatchDump()
    val expectedMatch1 = new Match()
    expectedMatch1.setEvents(ListBuffer(1, 2))
    expectedMatch1.setFull(true)
    val expectedMatch2 = new Match()
    expectedMatch2.setEvents(ListBuffer(3, 4))
    expectedMatch2.setFull(true)
    md.addMatch(expectedMatch1)
    md.addMatch(expectedMatch2)

    val dfaProvider = DFAProvider(DFASourceRegExp(mypattern, policy, 0, es.getEventTypes))
    val fsmp = FSMProvider(dfaProvider)
    val erf = ERFTask(fsmp, ss, show = false)
    val prof = erf.execute()
    prof.printMatches()

    assert(prof.getMatchDump.checkAgainst(md))
  }
}
