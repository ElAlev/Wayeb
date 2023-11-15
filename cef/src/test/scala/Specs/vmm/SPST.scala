package Specs.vmm

import com.typesafe.scalalogging.LazyLogging
import fsm.SPSTInterface
import fsm.symbolic.logic.{PredicateConstructor, Sentence}
import fsm.symbolic.sre.SREUtils
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.StreamFactory
import utils.testing.PatternGenerator
import model.vmm.VMMUtils
import ui.ConfigUtils
import workflow.provider.SDFAProvider
import workflow.provider.source.sdfa.SDFASourceFormula

@RunWith(classOf[JUnitRunner])
class SPST extends FlatSpec with LazyLogging {
  "Distributions estimated with exhaustive calculation" should " be the same as those estimated with optimized method " in {
    for (k <- 1 to ConfigUtils.maxOrder) testPatterns(k)
  }

  def testPatterns(k: Int): Unit = {
    require(k > 0)
    val patterns = PatternGenerator.generateRegExpPatterns(
      ConfigUtils.noOfPatterns,
      Set("A", "B", "C"),
      ConfigUtils.patternMaxDepth
    )
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusives = Set(Set(pa, pb, pc))
    val extras = Set.empty[Sentence]

    for (pattern <- patterns) {
      val formula = SREUtils.re2formula(pattern)
      logger.debug("Testing equivalence for pattern/formula: " + pattern.toString + "/" + formula.toString)
      val sdfap = SDFAProvider(
        SDFASourceFormula(
          List((formula, 0, ConfigUtils.singlePartitionVal, -1, "count")),
          ConfigUtils.defaultPolicy,
          exclusives,
          extras
        )
      )
      val sdfa = sdfap.provide().head.sdfa
      val streamSource = StreamFactory.getStreamSource(
        10000,
        scala.collection.mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5),
        10
      )
      val maxOrder = 3
      val pMin = 0.0001
      val alpha = 0.0
      val gamma = 0.001
      val r = 1.05
      val (pst, iso) = VMMUtils.learnPredictionSuffixTree(
        streamSource, sdfa, ConfigUtils.singlePartitionVal, maxOrder, pMin, alpha, gamma, r
      )
      val spsti = SPSTInterface(pst, sdfa, iso, maxOrder, 0, ConfigUtils.singlePartitionVal)
      val horizon = 3
      val wtEx = spsti.computeWtDistsExhaustive(horizon)
      val wt = spsti.computeWtDistsOpt(horizon, 0.0)
      logger.debug("Distribution with exhaustive " + wtEx)
      logger.debug("Distribution optimized " + wt)
      val exindices = wtEx.keySet
      val indices = wt.keySet
      assert(exindices == indices)
      assert(indices.forall(i => wt(i).compareAgainst(wtEx(i))))
    }
  }

}
