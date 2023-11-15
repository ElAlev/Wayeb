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
class NSRAeqSREM extends FlatSpec with LazyLogging{
  "NSRA" should "accept same words as corresponding SREM" in {
    simpleTest1()
    simpleTest2()
    testPatterns()
  }

  def simpleTest2(): Unit = {
    val home = System.getenv("WAYEB_HOME")
    val fn = home + "/patterns/validation/pattern4.sre" //"/patterns/bio/patternNotInteresting.sre"
    val reader = new FileReader(fn)
    val parsed = parseAll(formulasList, reader)
    val f = parsed.get
    val formula = f.head._1

    val eventsAccepted = List(
      GenericEvent(1, "B", 1, Map("xattr" -> 3.0)),
      GenericEvent(2, "A", 2, Map("xattr" -> 3.0))
    )
    val formulaAccepts = formula.accepts(eventsAccepted)
    assert(!formulaAccepts)
    val eventsRejected = List(
      GenericEvent(1, "B", 1, Map("xattr" -> 3.0)),
      GenericEvent(2, "A", 2, Map("xattr" -> 4.0))
    )
    val formulaRejects = formula.accepts(eventsRejected)
    assert(!formulaRejects)
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
    val eventsRejected = List(
      GenericEvent(1,"A",1,Map("xattr" -> 3.0)),
      GenericEvent(2,"B",2,Map("xattr" -> 4.0))
    )
    val formulaRejects = formula.accepts(eventsRejected)
    assert(!formulaRejects)

    val nsra = NSRAUtils.buildNSRA(formula)
    val nsraAccepts = nsra.accepts(eventsAccepted)
    assert(formulaAccepts == nsraAccepts)
    val nsraRejects = nsra.accepts(eventsRejected)
    assert(formulaRejects == nsraRejects)
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
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
      logger.debug("Testing NSRA-SREM equivalence for pattern/formula: " + p.toString + "/" + formula.toString)
      val nsra = NSRAUtils.buildNSRA(formula)
      for (word <- words) {
        logger.debug("...with word " + word)
        val events = word.map(
          c => {
            val spl = c.split(",")
            GenericEvent(0, spl(0), 0, Map("attr" -> spl(1)))
          }
        )
        val nsraAccepts = nsra.accepts(events)
        val sremAccepts = formula.accepts(events)
        logger.debug("\t\t Result nsra/srem: " + nsraAccepts + "/" + sremAccepts)
        assert(nsraAccepts == sremAccepts)
      }
    }

  }
}
