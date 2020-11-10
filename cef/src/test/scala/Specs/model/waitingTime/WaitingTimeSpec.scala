package Specs.model.waitingTime

import breeze.stats.distributions._
import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy._
import fsm.DFAInterface
import fsm.classical.fa.dfa.DFAFactory
import fsm.classical.pattern.regexp.{RegExpTree, RegExpUtils}
import model.markov.{MarkovChainFactory, TransitionProbs}
import model.waitingTime.{ForecastMethod, WtDistribution}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import ui.ConfigUtils
import scala.collection.mutable
import scala.util.control.Breaks._

@RunWith(classOf[JUnitRunner])
class WaitingTimeSpec extends FlatSpec with LazyLogging {
  "Smart scan " should " produce same forecast intervals as full scan" in {

    val maxSpread = 100
    val confidenceThreshold = 0.3

    logger.debug("Testing with manually constructed distribution")
    val wt1 = mutable.Map[Int, Double]()
    wt1 += (1 -> 0.0)
    wt1 += (2 -> 0.2)
    wt1 += (3 -> 0.3)
    wt1 += (4 -> 0.1)
    wt1 += (5 -> 0.1)
    wt1 += (6 -> 0.1)
    wt1 += (7 -> 0.1)
    wt1 += (8 -> 0.05)
    wt1 += (9 -> 0.05)
    wt1 += (10 -> 0.0)
    val wtd1 = WtDistribution(wt1.toMap)
    assert(testMethods(wtd1, confidenceThreshold, wt1.size))

    logger.debug("Testing with known distributions")
    assert(testDistribution("gaussian", confidenceThreshold, maxSpread))
    assert(testDistribution("poisson", confidenceThreshold, maxSpread))

    logger.debug("Testing with DFAs.")
    val tprobs = new TransitionProbs
    tprobs.addProb(List("a"), 0.20)
    tprobs.addProb(List("b"), 0.20)
    tprobs.addProb(List("c"), 0.20)
    tprobs.addProb(List("d"), 0.20)
    tprobs.addProb(List("e"), 0.20)
    tprobs.addMarginal("a", 0.20)
    tprobs.addMarginal("b", 0.20)
    tprobs.addMarginal("c", 0.20)
    tprobs.addMarginal("d", 0.20)
    tprobs.addMarginal("e", 0.20)
    val orpat = RegExpUtils.getUnionStr(List("a","b","c","d","e"))
    assert(testDFA(orpat, tprobs, confidenceThreshold, maxSpread))

    tprobs.clear()
    tprobs.addProb(List("a"), 0.30)
    tprobs.addProb(List("b"), 0.30)
    tprobs.addProb(List("c"), 0.40)
    tprobs.addMarginal("a", 0.30)
    tprobs.addMarginal("b", 0.30)
    tprobs.addMarginal("c", 0.40)
    val cbapat = RegExpUtils.getConcatStr(List("c", "b", "a"))
    assert(testDFA(cbapat, tprobs, confidenceThreshold, maxSpread))
  }

  def testDFA(
               pat: RegExpTree,
               probs: TransitionProbs,
               predThres: Double,
               maxSpread: Int
             ): Boolean = {
    val inputSymbols = probs.getSymbols
    val dfa = DFAFactory.buildDFAFromRe(pat, NONOVERLAP, inputSymbols.toSet, 0)
    val fsm = DFAInterface(dfa)
    val mc = MarkovChainFactory.buildMC(fsm, probs)
    logger.debug("Testing DFA " + pat)
    val wtds = mc.computeWTDists(fsm,horizon = 200, ConfigUtils.defaultFinalsEnabled)
    wtds.forall(wtd => testMethods(wtd._2,predThres,maxSpread))
  }

  def testDistribution(
                        which: String,
                        predThres: Double,
                        maxSpread: Int
                      ): Boolean = {
    var pass = false
    if (which.equalsIgnoreCase("gaussian"))
      pass = testGaussian(predThres, maxSpread)
    else if (which.equalsIgnoreCase("poisson"))
      pass = testPoisson(predThres, maxSpread)
    pass
  }

  def testPoisson(
                   predThres: Double,
                   maxSpread: Int
                 ): Boolean = {
    var pass = true
    val wt = mutable.Map[Int, Double]()
    val lamd = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    logger.debug("Testing Poisson")
    breakable {
      for (l <- lamd) {
        logger.debug("\t with lambda: " + l)
        val poi = new Poisson(l)
        for (s <- 1 to 500)
          wt += (s -> poi.probabilityOf(s))
        val wtd = WtDistribution(wt.toMap)
        if (!testMethods(wtd, predThres, maxSpread)) {
          pass = false
          break
        }
      }
    }
    pass
  }

  def testGaussian(
                    predThres: Double,
                    maxSpread: Int
                  ): Boolean = {
    var pass = true
    val wt = mutable.Map[Int, Double]()
    val musigmas = mutable.Map[Int, Int]()
    musigmas += (5 -> 1)
    musigmas += (10 -> 3)
    musigmas += (25 -> 5)
    musigmas += (50 -> 10)
    logger.debug("Testing Gaussian")
    breakable {
      for ((k, v) <- musigmas) {
        logger.debug("\t with mu/sigma: " + k, v)
        val gauss = new Gaussian(k, v)
        for (s <- 1 to 500)
          wt += (s -> gauss.probability((s - 1).toDouble, s.toDouble))
        val wtd = WtDistribution(wt.toMap)
        if (!testMethods(wtd, predThres, maxSpread)) {
          pass = false
          break
        }
      }
    }
    pass
  }

  def testMethods(
                   wtd: WtDistribution,
                   predictionThreshold: Double,
                   maxSpread: Int
                 ): Boolean = {
    val fullScanPred = wtd.buildForecast(ForecastMethod.FULLSCAN, predictionThreshold, maxSpread)
    val smartScanPred = wtd.buildForecast(ForecastMethod.SMARTSCAN, predictionThreshold, maxSpread)
    logger.info("\nFull scan: " + fullScanPred + "\n Smart scan: " + smartScanPred)
    fullScanPred.start == smartScanPred.start & fullScanPred.end == smartScanPred.end
  }

}
