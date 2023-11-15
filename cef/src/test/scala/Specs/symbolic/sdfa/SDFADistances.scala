package Specs.symbolic.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.SDFAInterface
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre.SREUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import ui.ConfigUtils
import utils.testing.PatternGenerator

@RunWith(classOf[JUnitRunner])
class SDFADistances extends FlatSpec with LazyLogging {
  "Optimized calculation of shortest walk distances" should "be the same as exhaustive calculations" in {
    testPatterns()
  }

  def testPatterns(): Unit = {
    logger.debug("test patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing distances for pattern/formula: " + p.toString + "/" + formula.toString)
      val snfa = SNFAUtils.buildSNFA(formula)
      val sdfa = SFAUtils.determinizeI(snfa, Set(exclusive), Set.empty)
      val sdfai = SDFAInterface(sdfa)
      sdfai.estimateRemainingPercentage
      val optDistances = sdfai.shortestPathDistances
      logger.debug("SDFA: " + sdfa.toString)
      logger.debug("Optimized distances: " + optDistances.toString())
      val pathDistances = sdfai.findShortestPathDistancesPaths
      logger.debug("Exhaustive distances: " + pathDistances.toString())
      assert(optDistances == pathDistances)
    }
  }

}
