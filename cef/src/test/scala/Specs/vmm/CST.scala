package Specs.vmm

import breeze.stats.distributions.Uniform
import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import utils.testing.SymbolWordGenerator
import model.vmm.{Symbol, SymbolWord}
import model.vmm.pst.{CounterSuffixTree, CyclicBuffer}
import ui.ConfigUtils
import scala.math.pow

@RunWith(classOf[JUnitRunner])
class CST extends FlatSpec with LazyLogging {
  "A counter suffix tree" should "maintain proper counters " in {
    testab()
    testTreeWithWords()
    for (k <- 2 to ConfigUtils.maxOrder) testDistributions(k)
  }

  def testab(): Unit = {
    logger.debug("Testing aaabaabaaa")
    val m = 2
    val stream = Array(Symbol(1),Symbol(1),Symbol(1),Symbol(2),Symbol(1),Symbol(1),Symbol(2),Symbol(1),Symbol(1),Symbol(1))
    val buf = new CyclicBuffer(m)
    val cst = CounterSuffixTree()
    var symbols: Set[Symbol] = Set.empty
    for (i <- stream.indices) {
      buf.pushSymbol(stream(i))
      val bufferedWord = buf.pop
      if (bufferedWord.length == m) cst.updateWithNewWord(bufferedWord)
      symbols = symbols + stream(i)
    }
    logger.debug("Counter suffix tree: " + cst.toString)
    assert(cst.getCounterFor(List(Symbol(1))) == 7)
    assert(cst.getCounterFor(List(Symbol(2))) == 2)
    assert(cst.getCounterFor(List(Symbol(1),Symbol(1))) == 5)
    assert(cst.getCounterFor(List(Symbol(1),Symbol(2))) == 2)
    assert(cst.getCounterFor(List(Symbol(2),Symbol(1))) == 2)
  }

  def testDistributions(k: Int): Unit = {
    logger.debug("Testing generated distributions of counter suffix trees")
    val symbolsNo = ConfigUtils.symbolsNo
    val stream = SymbolWordGenerator.generateSymbolStream(symbolsNo, ConfigUtils.symbolStreamSize)
    val buf = new CyclicBuffer(k + 1)
    val cst = CounterSuffixTree()
    var symbols: Set[Symbol] = Set.empty
    for (i <- stream.indices) {
      buf.pushSymbol(stream(i))
      val bufferedWord = buf.pop
      if (bufferedWord.length == k+1) cst.updateWithNewWord(bufferedWord)
      symbols = symbols + stream(i)
    }
    logger.debug("Counter suffix tree: " + cst.toString)
    val noOfWords = pow(symbolsNo, k).toInt
    val words = SymbolWordGenerator.generateSymbolWords(symbols, k, noOfWords)
    val wordsWithCounters = words.filter(w => cst.getCounterFor(w.word) > 0)
    for (word <- wordsWithCounters) {
      logger.debug("Testing word " + word)
      val dist = cst.getSymbolDistributionFor(word.word)
      assert(dist.isConsistent)
      val dsum = dist.sum
      if (dsum < 1.0) {
        logger.debug("Found distribution with lower sum: " + dist)
        logger.debug("Sum is " + dsum)
        val smoothed = dist.smoothSer(ConfigUtils.gammaMin, symbols)
        logger.debug("Smoothed distribution: " + smoothed.toString)
        logger.debug("Sum is " + smoothed.sum)
      }
    }
  }

  /**
    * Test applicable only because the tree is built with words of the same length.
    */
  def testTreeWithWords(): Unit = {
    val words = SymbolWordGenerator.generateSymbolWords(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    for (i <- 1 to ConfigUtils.wordMaxLength) {
      val wordsi = words.filter(x => x.length == i)
      val wordsiWithCounters = scala.collection.mutable.Map.empty[SymbolWord, Int]
      val indexedWordsi = (1 to wordsi.size).zip(wordsi).toMap
      val totalWordsNo = 100
      val cst = CounterSuffixTree()
      val uni = new Uniform(1, wordsi.size)
      for (j <- 1 to totalWordsNo) {
        val sampleIndex = uni.sample()
        val word = indexedWordsi(sampleIndex.toInt)
        if (wordsiWithCounters.contains(word)) wordsiWithCounters(word) += 1
        else wordsiWithCounters += (word -> 1)
        cst.updateWithNewWord(word.word)
      }
      logger.debug("Size: " + cst.getSize + "\t symbols: " + cst.getSymbols + "\n" + cst)
      assert(cst.getCounterFor(List.empty) == totalWordsNo)
      for (w <- wordsiWithCounters) {
        val originalCounter = w._2
        val treeCounter = cst.getCounterFor(w._1.word)
        logger.debug("word " + w._1 + " original " + originalCounter + " tree " + treeCounter)
        assert(originalCounter == treeCounter)
      }
      assert(cst.consistencyCheck)
    }
  }

  //TODO: another consistency test in which the tree is constructed from a stream.

}
