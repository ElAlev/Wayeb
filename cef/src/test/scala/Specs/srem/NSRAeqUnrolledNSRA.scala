package Specs.srem

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sra.nsra.NSRAUtils
import fsm.symbolic.sre.ParseSREFormula$.{formulasList, parseAll}
import fsm.symbolic.sre.SREUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.GenericEvent
import ui.ConfigUtils
import utils.testing.{PatternGenerator, SymbolWordGenerator}
import java.io.FileReader

@RunWith(classOf[JUnitRunner])
class NSRAeqUnrolledNSRA  extends FlatSpec with LazyLogging {
  "NSRA " should " be equivalent to unrolled NSRA only for words smaller or equal to the window" in {
    simpleTest1()
    testPatterns()
  }

  def simpleTest1(): Unit = {
    val home = System.getenv("WAYEB_HOME")
    val fn = home + "/patterns/validation/pattern3.sre" //"/patterns/bio/patternNotInteresting.sre"
    val reader = new FileReader(fn)
    val parsed = parseAll(formulasList, reader)
    val f = parsed.get
    val formula = f.head._1

    val eventsAccepted = List(
      GenericEvent(1,"A",1,Map("xattr" -> 3.0)),
      GenericEvent(2,"B",2,Map("xattr" -> 3.0))
    )
    val formulaAccepts = formula.accepts(eventsAccepted)
    assert(formulaAccepts)

    val nsra = NSRAUtils.buildNSRA(formula)
    val elimNSRA = NSRAUtils.eliminateEpsilon(nsra)
    val unrolledNSRA2 = NSRAUtils.unroll(elimNSRA, 2)
    val unrolledNSRA1 = NSRAUtils.unroll(elimNSRA, 1)
    val elimNSRAAccepts = elimNSRA.accepts(eventsAccepted)
    val unrolledNSRA2Accepts = unrolledNSRA2.accepts(eventsAccepted)
    val unrolledNSRA1Accepts = unrolledNSRA1.accepts(eventsAccepted)
    assert(elimNSRAAccepts == unrolledNSRA2Accepts)
    assert(!unrolledNSRA1Accepts)
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
      logger.debug("Testing NSRA  - unrolled NSRA equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
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
          if (events.size > window) {
            logger.debug("\t\t Result unrolled nsra for window: " + unrolledNSRAAccepts + "/" + window)
            assert(!unrolledNSRAAccepts)
          }
          else {
            assert(nsraAccepts == elimNSRAAccepts)
            assert(elimNSRAAccepts == unrolledNSRAAccepts)
            logger.debug("\t\t Result nsra/unrolled nsra: " + elimNSRAAccepts + "/" + unrolledNSRAAccepts)
          }
        }
      }
    }
  }




}
