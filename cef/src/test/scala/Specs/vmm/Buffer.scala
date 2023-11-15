package Specs.vmm

import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import utils.testing.SymbolWordGenerator
import model.vmm.{Symbol, SymbolWord}
import model.vmm.pst.CyclicBuffer
import ui.ConfigUtils

@RunWith(classOf[JUnitRunner])
class Buffer extends FlatSpec with LazyLogging {
  "A buffer" should " return the words that have been stored in it " in {
    testWords()
    testStream()
  }

  def testStream(): Unit = {
    logger.debug("Testing buffer with stream")
    val stream = SymbolWordGenerator.generateSymbolStream(ConfigUtils.symbolsNo, ConfigUtils.symbolStreamSize)
    val k = ConfigUtils.wordMaxLength
    val buf = new CyclicBuffer(k)
    logger.debug(stream.toList.toString())
    for (i <- stream.indices) {
      buf.pushSymbol(stream(i))
      val lastK = getLastKFromStream(stream, k, i)
      logger.debug("From position " + i + ": \t" + lastK)
      val buffered = buf.pop
      logger.debug("Buffered: \t" + buffered)
      assert(buffered == lastK)
    }

  }

  def getLastKFromStream(
                          s: Array[Symbol],
                          k: Int,
                          head: Int
                        ): List[Symbol] = {
    require(k > 0 & k <= s.length & head >= 0 & head < s.length)
    val up2Head = s.take(head + 1)
    val kBeforeHead = if (k > up2Head.length) up2Head else up2Head.drop(up2Head.length - k)
    kBeforeHead.toList.reverse
  }

  def testWords(): Unit = {
    logger.debug("Testing buffer with words")
    val words = SymbolWordGenerator.generateSymbolWords(Set("A", "B", "C"), ConfigUtils.wordMaxLength)
    val wordsl = words.filter(x => x.length == ConfigUtils.wordMaxLength)
    val buf = new CyclicBuffer(ConfigUtils.wordMaxLength)
    for (word <- wordsl) {
      pushWord(buf, word)
      val bufferedWord = buf.pop
      logger.debug(bufferedWord + "\t" + word)
      assert(bufferedWord == word.word)
      buf.pushWord(word.word)
      assert(buf.pop == word.word)
    }
  }

  def pushWord(
                b: CyclicBuffer,
                w: SymbolWord
              ): Int = {
    val l = w.word.reverse
    pushWord(b, l, -1)
  }

  def pushWord(
                b: CyclicBuffer,
                l: List[Symbol],
                h: Int
              ): Int = {
    l match {
      case head :: tail => {
        val newHead = b.pushSymbol(head)
        pushWord(b, tail, newHead)
      }
      case Nil => h
    }
  }

}
