package Specs.vmm

import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import utils.testing.SymbolWordGenerator
import model.vmm.{Symbol, SymbolWord}
import model.vmm.pst.psa.PSAUtils
import model.vmm.pst.{CounterSuffixTree, CyclicBuffer, PSTLearner}

@RunWith(classOf[JUnitRunner])
class PST2PSA extends FlatSpec with LazyLogging {
  "PSA built from PST" should "be transition proper" in {
    testTransitions()
  }

  "PSA built from PST" should "be probabilistically equivalent to PST" in {
    testEquivalence()
  }

  def testEquivalence(): Unit = {
    logger.debug("Equivalence test for PST to PSA conversion")
    val orders: List[Int] = List(1, 2, 3, 4, 5)
    val streamsNo: Int = 10
    val contextsNo = 50
    val r = scala.util.Random

    for (
      stream <- 1 to streamsNo;
      k <- orders
    ) {
      val stream = SymbolWordGenerator.generateSymbolStream(5, 10000)
      //val k = 3
      val buf = new CyclicBuffer(k + 1)
      val cst = CounterSuffixTree()
      var symbols: Set[Symbol] = Set.empty
      for (i <- stream.indices) {
        buf.pushSymbol(stream(i))
        val bufferedWord = buf.pop
        cst.updateWithNewWord(bufferedWord)
        symbols = symbols + stream(i)
      }
      val pstl = PSTLearner(symbols, k, 1000)
      val pstf = pstl.learnOriginal(cst, withMissing = false)
      val pstt = pstl.learnOriginal(cst, withMissing = true)
      val psta = pstt.makePSACompatible()
      val psa = PSAUtils.buildPSA(psta)

      for (c <- 1 to contextsNo) {
        val contextLength = 1 + r.nextInt(10)
        val context = SymbolWordGenerator.generateSymbolStream(5, contextLength).toList
        val canStart = psa.canStartWith(SymbolWord(context))
        canStart match {
          case Some(x) => {
            for (symbol <- symbols) {
              val psaprob = x._2.getProbFor(symbol)
              val pstprob = pstf.getConditionalProbFor(symbol, context)
              logger.debug("PSA prob, symbol=" + symbol + " context=" + context + " : " + psaprob)
              logger.debug("PST prob, symbol=" + symbol + " context=" + context + " : " + pstprob)
              val psaprobInt = (psaprob * 1000).toInt
              val pstprobInt = (pstprob * 1000).toInt
              assert(psaprobInt == pstprobInt)
            }
          }
          case None => logger.debug("Skipping " + context)
        }
      }
    }

  }

  def testTransitions(): Unit = {
    logger.debug("Transition test for PST")
    val orders: List[Int] = List(1, 2, 3, 4, 5)
    val streamsNo: Int = 10

    for (
      stream <- 1 to streamsNo;
      k <- orders
    ) {
      val stream = SymbolWordGenerator.generateSymbolStream(5, 10000)
      //val k = 10
      val buf = new CyclicBuffer(k + 1)
      val cst = CounterSuffixTree()
      var symbols: Set[Symbol] = Set.empty
      for (i <- stream.indices) {
        buf.pushSymbol(stream(i))
        val bufferedWord = buf.pop
        cst.updateWithNewWord(bufferedWord)
        symbols = symbols + stream(i)
      }
      val pstl = PSTLearner(symbols, k, 1000)

      val pstf = pstl.learnOriginal(cst, withMissing = false)
      logger.debug("PSTf size " + pstf.getSize)
      val pstfIsTransProper = pstf.isTransitionProper
      logger.debug("PSTf is transition proper " + pstfIsTransProper)

      val pstt = pstl.learnOriginal(cst, withMissing = true)
      logger.debug("PSTt size " + pstt.getSize)
      val psttIsTransProper = pstt.isTransitionProper
      logger.debug("PSTt is transition proper " + psttIsTransProper)
      assert(psttIsTransProper)

      val pstExpanded = pstt.makePSACompatible()
      logger.debug("PST expanded size " + pstExpanded.getSize)
      val pstExpandedIsTransProper = pstExpanded.isTransitionProper
      logger.debug("PST expanded is transition proper " + pstExpandedIsTransProper)
      assert(pstExpandedIsTransProper)

      val pMin = 0.001
      val alpha = 0.01
      val gamma = 0.001
      val r = 1.05
      val pstNewLearner = PSTLearner(symbols, k, pMin, alpha, gamma, r)
      val psttNew = pstNewLearner.learnVariant(cst, withMissing = true)
      logger.debug("PSTtNew size " + psttNew.getSize)
      val psttNewIsTransProper = psttNew.isTransitionProper
      logger.debug("PSTtNew is transition proper " + psttNewIsTransProper)
      assert(psttNewIsTransProper)

      val pstNewExpanded = psttNew.makePSACompatible()
      logger.debug("PST new expanded size " + pstNewExpanded.getSize)
      val pstNewExpandedIsTransProper = pstNewExpanded.isTransitionProper
      logger.debug("PST New expanded is transition proper " + pstNewExpandedIsTransProper)
      assert(pstNewExpandedIsTransProper)
    }
  }

}
