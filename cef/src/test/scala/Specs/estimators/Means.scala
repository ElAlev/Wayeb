package Specs.estimators

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.PredicateConstructor
import fsm.symbolic.sre.{LogicAtomicSentence, LogicConstant, LogicPredicate, RegularOperator, SREOperator, SRESentence, SREUtils}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.StreamFactory
import ui.ConfigUtils
import utils.testing.PatternGenerator
import workflow.provider.source.forecaster.ForecasterSourceBuild
import workflow.provider.source.rt.RTSourceDirect
import workflow.provider.{FSMProvider, ForecasterProvider, RemainingTimesProvider, SDFAProvider, WtProvider}
import workflow.provider.source.sdfa.SDFASourceFormula
import workflow.provider.source.wt.WtSourceRT
import workflow.task.engineTask.ERFTask
import workflow.task.estimatorTask.MeanTask

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class Means extends FlatSpec with LazyLogging {
  "Actual mean value from remaining times " should "be close to rounded mean value from forecasting " in {
    simpleTest()
    testPatterns()
  }

  def simpleTest(): Unit = {
    logger.debug("simple test")
    val sa = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("A"))))
    val sb = SRESentence(LogicAtomicSentence(LogicPredicate("IsEventTypePredicate"), List(LogicConstant("B"))))
    val ab = SREOperator(RegularOperator.SEQ, List(sa, sb))
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val exclusive = Set(pa, pb)
    val formula = ab
    val sdfap = SDFAProvider(SDFASourceFormula(List((formula, 0, "$", -1, "count")), ConfigUtils.defaultPolicy, Set(exclusive), Set.empty))
    val sdfa = sdfap.provide().head
    val fsmp = FSMProvider(sdfap)
    val ss = StreamFactory.getStreamSource(100, mutable.Map("A" -> 0.25, "B" -> 0.75), 10)
    val meansTask = MeanTask(fsmp, ss)
    val rts = meansTask.execute()._1
    logger.debug("SDFA: " + sdfa.toString)
    logger.debug("Remaining times: " + rts.toString())
    val rtp = RemainingTimesProvider(RTSourceDirect(rts))
    val horizon = 200
    val predThres = 0.0
    val maxSpread = 0
    val wtdProv = WtProvider(WtSourceRT(fsmp, rtp, horizon, ConfigUtils.defaultFinalsEnabled))
    val pp = ForecasterProvider(ForecasterSourceBuild(fsmp, wtdProv, horizon, predThres, maxSpread))
    val predictors = pp.provide()

    val rt = rts.head
    val pred = predictors.head

    val states = rt.getStates
    val close = states.forall(x => {
      val mud = rt.getMuForState(x)
      val mui = pred.getNewForecast(x, 0).startRelativeToNow
      val isClose = checkIfClose(mud, mui)
      if (!isClose) logger.debug("Not close: " + x + "/" + mud + "/" + mui)
      isClose
    })
    assert(close)
    val erf = ERFTask(fsmp, pp, ss, show = false)
    val prof = erf.execute()
    prof.printProfileInfo()

  }

  def testPatterns(): Unit = {
    logger.debug("testing multiple random patterns")
    val patterns = PatternGenerator.generateRegExpPatterns(ConfigUtils.noOfPatterns, Set("A", "B", "C"), ConfigUtils.patternMaxDepth)
    val ss = StreamFactory.getStreamSource(1000, mutable.Map("A" -> 0.25, "B" -> 0.25, "C" -> 0.5), 10)
    val pa = PredicateConstructor.getEventTypePred("A")
    val pb = PredicateConstructor.getEventTypePred("B")
    val pc = PredicateConstructor.getEventTypePred("C")
    val exclusive = Set(pa, pb, pc)
    for (p <- patterns) {
      val formula = SREUtils.re2formula(p)
      logger.debug("Testing means for pattern/formula: " + p.toString + "/" + formula.toString)
      val sdfap = SDFAProvider(SDFASourceFormula(List((formula, 0, "$", -1, "count")), ConfigUtils.defaultPolicy, Set(exclusive), Set.empty))
      val fsmp = FSMProvider(sdfap)
      val meansTask = MeanTask(fsmp, ss)
      val rts = meansTask.execute()._1
      logger.debug("Remaining times: " + rts.toString())

      val rtp = RemainingTimesProvider(RTSourceDirect(rts))
      val horizon = 200
      val predThres = 0.0
      val maxSpread = 0
      val wtdProv = WtProvider(WtSourceRT(fsmp, rtp, horizon, ConfigUtils.defaultFinalsEnabled))
      val pp = ForecasterProvider(ForecasterSourceBuild(fsmp, wtdProv, horizon, predThres, maxSpread))
      val predictors = pp.provide()

      val rt = rts.head
      val pred = predictors.head

      val states = rt.getStates
      val close = states.forall(x => {
        val mud = rt.getMuForState(x)
        val mui = pred.getNewForecast(x, 0).startRelativeToNow
        val isClose = checkIfClose(mud, mui)
        if (!isClose) logger.debug("Not close: " + x + "/" + mud + "/" + mui)
        isClose
      })
      assert(close)
    }
  }

  private def checkIfClose(
                            d: Double,
                            i: Int
                          ): Boolean = {
    val f = math.floor(d)
    val c = math.ceil(d)
    (i == f) | (i == c)
  }

}
