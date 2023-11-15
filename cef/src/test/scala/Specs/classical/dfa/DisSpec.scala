package Specs.classical.dfa

import com.typesafe.scalalogging.LazyLogging
import fsm.DFAInterface
import fsm.classical.fa.dfa.DFAUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.source.EmitMode
import stream.StreamFactory
import ui.ConfigUtils
import utils.testing.PatternGenerator
import workflow.provider.source.dfa.DFASourceRegExp
import workflow.provider.{DFAProvider, FSMProvider}
import workflow.task.engineTask.ERFTask
import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class DisSpec extends FlatSpec with LazyLogging {
  "Disambiguated DFAs " should " produce same match dumps as 0-order DFAs " in {
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("a", "b", "c"), ConfigUtils.patternMaxDepth)
    val ss = StreamFactory.getStreamSource(1000, mutable.Map("a" -> 0.25, "b" -> 0.25, "c" -> 0.25, "d" -> 0.25), 10)
    val es = ss.emitEventsAndClose(EmitMode.BUFFER)
    val policy = ConfigUtils.defaultPolicy
    val maxM = ConfigUtils.maxOrder
    for (pat <- patterns) {
      logger.debug("\nTesting pattern: " + pat)
      val dfaProvider = DFAProvider(DFASourceRegExp(pat, policy, 0, es.getEventTypes))
      val fsmp = FSMProvider(dfaProvider)
      val erf = ERFTask(fsmp, ss, show = false, reset = false)
      val prof0 = erf.execute()
      for (m <- 1 to maxM) {
        logger.debug("\nTesting pattern: " + pat + " @order " + m)
        val dfaProvider = DFAProvider(DFASourceRegExp(pat, policy, m, es.getEventTypes))
        val fsmp = FSMProvider(dfaProvider)
        val erf = ERFTask(fsmp, ss, show = false, reset = false)
        val profm = erf.execute()
        assert(DFAUtils.isMUnambiguous(erf.getEngine.getFSMs.head.asInstanceOf[DFAInterface].dfa, m))
        assert(prof0.getMatchDump.checkAgainst(profm.getMatchDump))
      }
    }
  }
}
