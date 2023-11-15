package Specs.srem

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sra.SRAUtils
import fsm.symbolic.sra.nsra.NSRAUtils
import fsm.symbolic.sre.SREUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}

@RunWith(classOf[JUnitRunner])
class NSRAeqWDSRA extends FlatSpec with LazyLogging {
  "NSRA " should " be equivalent to windowed DSRA" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfWPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth,
      Set("x","y")
    )
    val words = SymbolWordGenerator.generateStringsFromSymbolsValues(
      Set("A", "B", "C"),
      ConfigUtils.wordMaxLength,
      Set(1, 2)
    )
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing NSRA  - windowed DSRA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val nsra = NSRAUtils.buildNSRA(formula)
      val elimNSRA = NSRAUtils.eliminateEpsilon(nsra)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(
          c => {
            val spl = c.split(",")
            GenericEvent(0, spl(0), 0, Map("attr" -> spl(1)))
          }
        )
        val nsraAccepts = nsra.accepts(events)
        val elimNSRAAccepts = elimNSRA.accepts(events)
        for (window <- 1 to ConfigUtils.maxUnrollingWindow) {
          val unrolledNSRA = NSRAUtils.unroll(elimNSRA, window)
          val unrolledNSRAAccepts = unrolledNSRA.accepts(events)
          val dsra = SRAUtils.determinizeUnrolled(unrolledNSRA)
          val dsraAccepts = dsra.accepts(events)
          if (events.size > window) {
            logger.debug("\t\t Result unrolled nsra for window: " + unrolledNSRAAccepts + "/" + window)
            assert(!unrolledNSRAAccepts)
          }
          else {
            assert(nsraAccepts == elimNSRAAccepts)
            assert(elimNSRAAccepts == unrolledNSRAAccepts)
            assert(unrolledNSRAAccepts == dsraAccepts)
            logger.debug("\t\t Result unrolled nsra / windowed dsra: " + unrolledNSRAAccepts + "/" + dsraAccepts)
          }
        }
      }
    }
  }

}
