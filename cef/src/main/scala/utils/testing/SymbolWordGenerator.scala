package utils.testing

import breeze.stats.distributions.Uniform
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Utils for randomly generating strings and symbol words.
  */
object SymbolWordGenerator {
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

}
