package utils.testing

import breeze.stats.distributions.Uniform
import model.vmm.{Symbol, SymbolWord}
import utils.SetUtils
import utils.StringUtils.list2Str

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.pow

/**
  * Utils for randomly generating strings and symbol words.
  */
object SymbolWordGenerator {

  def generateStringsFromSymbolsValues(
                                        symbols: Set[String],
                                        maxLength: Int,
                                        values: Set[Int]
                                      ): Set[List[String]] = {
    val valuesStr = values.map(v => v.toString)
    val product = SetUtils.cartesian[String](Set(symbols,valuesStr))
    val productStrings: Set[String] = product.map(x => list2Str[String](x.toList))
    generateStrings(productStrings, maxLength)
  }

  /**
    * From a set of symbols, creates all possible strings of length 1 up to maxLength.
    *
    * @param symbols The set os symbols (each symbol is a Scala string).
    * @param maxLength The maximum length.
    * @return A set with all strings of length up to maxLength. Each string represented as a list.
    */
  def generateStrings(
                       symbols: Set[String],
                       maxLength: Int
                     ): Set[List[String]] = {
    val patternsArray = generateStringsArray(symbols, maxLength)
    val patterns: Set[List[String]] = patternsArray.toSet
    patterns
  }

  /**
    * From a set of symbols, creates all possible strings of length 1 up to maxLength.
    *
    * @param symbols The set os symbols (each symbol is a Scala string).
    * @param maxLength The maximum length.
    * @return An array buffer with all strings of length up to maxLength. Each string represented as a list.
    */
  private def generateStringsArray(
                                    symbols: Set[String],
                                    maxLength: Int
                                  ): ArrayBuffer[List[String]] = {
    require(symbols.nonEmpty)
    require(maxLength > 0 & maxLength <= 10)
    val patterns = ArrayBuffer.empty[List[String]]
    patterns += List.empty[String]
    var patternsK = ArrayBuffer.empty[List[String]]
    for (i <- 1 to maxLength) {
      for (p <- patterns) {
        for (s <- symbols) {
          val newp = p ::: List(s)
          patternsK += newp
          /*if (withGaps & p.nonEmpty) {
            val newpdashed = p ::: List("#") ::: List(s)
            patternsK += newpdashed
          }*/
        }
      }
      patterns ++= patternsK
    }
    patterns.remove(0) // remove empty string
    patterns
  }

  /**
    * From a set of symbols, first creates all possible strings of length 1 up to maxLength and then retains only a
    * random percentage of them.
    *
    * @param symbols The set os symbols (each symbol is a Scala string).
    * @param maxLength The maximum length.
    * @param retainPercentage The percentage to be retained.
    * @return A set with all strings of length up to maxLength. Each string represented as a list.
    */
  def generateStrings(
                       symbols: Set[String],
                       maxLength: Int,
                       retainPercentage: Double
                     ): Set[List[String]] = {
    require(retainPercentage > 0.0 & retainPercentage <= 1.0)
    val allPatterns = generateStringsArray(symbols, maxLength)
    val allNo = allPatterns.size
    val uni = new Uniform(0, allNo - 1)
    val retainNo = (retainPercentage * allNo).toInt
    val s = uni.sample(retainNo)
    val retainedPatterns = mutable.Set.empty[List[String]]
    for (i <- s) retainedPatterns += allPatterns(i.toInt)
    retainedPatterns.toSet
  }

  /**
    * From a set of symbols, creates all possible words of length 1 up to maxLength.
    *
    * @param symbols The set os symbols (each symbol is a Scala string).
    * @param maxLength The maximum length.
    * @return A set with all words of length up to maxLength.
    */
  def generateSymbolWords(
                           symbols: Set[String],
                           maxLength: Int
                         ): Set[SymbolWord] = {
    val words = generateStrings(symbols, maxLength)
    val charsInts = symbols.zipWithIndex.toMap
    words.map(x => str2word(x, charsInts))
  }

  /**
    * Converts a string to a symbol word.
    *
    * @param s The string.
    * @param mapper A map from strings to ints to be used as symbol values.
    * @return The symbol word.
    */
  private def str2word(
                        s: List[String],
                        mapper: Map[String, Int]
                      ): SymbolWord = {
    val l = s.map(x => Symbol(mapper(x)))
    SymbolWord(l)
  }

  /**
    * Generates a stream of random symbols.
    *
    * @param symbolsNo The total number of symbols.
    * @param streamSize The stream size.
    * @return An array of random symbols.
    */
  def generateSymbolStream(
                            symbolsNo: Int,
                            streamSize: Int
                          ): Array[Symbol] = {
    require(symbolsNo > 0 & streamSize > 0)
    val stream = new Array[Symbol](streamSize)
    val uni = new Uniform(1, symbolsNo + 1)
    for (i <- 0 until streamSize) {
      val newSymbol = Symbol(uni.sample().toInt)
      stream(i) = newSymbol
    }
    stream
  }

  /**
    * Randomly generates a number of words of length up to maxLength from a given set of symbols.
    *
    * @param symbols The given set of symbols.
    * @param maxLength The maximum length.
    * @param noOfWords The total numner of words to be generated.
    * @return The set of random words.
    */
  def generateSymbolWords(
                           symbols: Set[Symbol],
                           maxLength: Int,
                           noOfWords: Int
                         ): Set[SymbolWord] = {
    require(maxLength > 0)
    require(noOfWords > 0 & noOfWords <= possibleWords(symbols, maxLength))
    require(symbols.nonEmpty)
    val symbolsList = symbols.toList
    var words = Set.empty[SymbolWord]
    while (words.size < noOfWords) {
      val newWord = generateSymbolWordUpToLength(symbolsList, maxLength)
      words = words + newWord
    }
    words
  }

  /**
    * The maximum possible number of words of length up to maxLength that can be generated from a given set of symbols.
    *
    * @param symbols The given set of symbols.
    * @param maxLength The maximum length.
    * @return The maximum number of possible words.
    */
  private def possibleWords(
                             symbols: Set[Symbol],
                             maxLength: Int
                           ): Int = {
    val sum = (1 to maxLength).map(x => pow(symbols.size, x).toInt).sum
    sum
  }

  /**
    * Generates a random word of length up to maxLength.
    *
    * @param symbols The set of symbols from which to build the word.
    * @param maxLength The word's maximum length.
    * @return The random word.
    */
  private def generateSymbolWordUpToLength(
                                            symbols: List[Symbol],
                                            maxLength: Int
                                          ): SymbolWord = {
    val length = sampleLength(maxLength)
    val word = generateSymbolWordOfLength(symbols, length, List.empty)
    SymbolWord(word)
  }

  /**
    * Recursively generates a random word of given length.
    *
    * @param symbols The symbols from which to draw a new symbol at every iteration.
    * @param remaining The remaining number of symbols to be added (should be equal to length at first call).
    * @param currentWord The word generated thus far.
    * @return The random word.
    */
  @scala.annotation.tailrec
  private def generateSymbolWordOfLength(
                                          symbols: List[Symbol],
                                          remaining: Int,
                                          currentWord: List[Symbol]
                                        ): List[Symbol] = {
    if (remaining == 0) currentWord
    else generateSymbolWordOfLength(symbols, remaining - 1, sampleSymbol(symbols) :: currentWord)
  }

  /**
    * Randomly picks a symbol from a list of symbols.
    *
    * @param symbols The given list of symbols.
    * @return The randomly chosen symbol.
    */
  private def sampleSymbol(symbols: List[Symbol]): Symbol = {
    val uni = new Uniform(0, symbols.size)
    val s = uni.sample().toInt
    symbols(s)
  }

  /**
    * Randomly picks an int from 1 up to maxLength.
    *
    * @param maxLength The max int.
    * @return the randomly chosen int.
    */
  private def sampleLength(maxLength: Int): Int = {
    require(maxLength > 0)
    val uni = new Uniform(1, maxLength + 1)
    uni.sample().toInt
  }
}
