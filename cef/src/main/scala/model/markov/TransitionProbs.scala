package model.markov

import scala.collection.mutable

/**
  * This class is used to hold a set of conditional probabilities on a set of symbols.
  * From this set of conditional probabilities, we can create a Markov chain.
  * We can also generate a stream. See stream.array.TransProbStream.
  * Used for testing.
  */
class TransitionProbs {
  // probs holds the conditional probabilities. An entry (l: List[String], p: Double) must be interpreted as
  // a conditional probability where the first l.size - 1 elements of l are the condition and its last element
  // the symbol for which we store the probability. For example, if l = List("a","b,"c"), then p = P(c|ab)
  private val probs: mutable.Map[List[String], Double] = mutable.Map.empty[List[String], Double]
  // holds the marginal probabilities for every symbol
  private val marginals: mutable.Map[String, Double] = mutable.Map.empty[String, Double]
  // order is the the length of the conditions/labels. If l = List("a","b,"c"), then order=2.
  // All labels must have the same length.
  private var order = 0

  /**
    * Returns a conditional probability.
    *
    * @param label The label/condition of the probability.
    * @param symbol The symbol.
    * @return The probability of the symbol given the label.
    */
  def getProb(
               label: List[String],
               symbol: String
             ): Double = {
    require(label.size == order)
    val key = label ::: List(symbol)
    probs(key)
  }

  /**
    * Returns the marginal probability of a symbol.
    *
    * @param symbol The symbol.
    * @return Its marginal probability.
    */
  def getMarginal(symbol: String): Double = {
    require(marginals.contains(symbol))
    marginals(symbol)
  }

  /**
    * @return All conditional probabilities.
    */
  def getAllProbs: mutable.Map[List[String], Double] = probs

  /**
    * @return only the non-zero probabilities
    */
  def getAllNonZeroProbs: mutable.Map[List[String], Double] = probs.filter(t => t._2 > 0.0)

  /**
    * Adds a new marginal probability.
    *
    * @param symbol The symbol for the probability.
    * @param m The probability.
    */
  def addMarginal(
                   symbol: String,
                   m: Double
                 ): Unit = {
    require(!marginals.contains(symbol))
    require(marginalConsistent(m))
    marginals += (symbol -> m)
  }

  /**
    * Adds a new conditional probability.
    *
    * @param s The symbol and the conditional.
    * @param p The probability.
    */
  def addProb(
               s: List[String],
               p: Double
             ): Unit = {
    require(s.nonEmpty)
    require(probConsistent(s, p))
    probs += (s -> p)
  }

  /**
    * Clear all probabilities and marginals.
    */
  def clear(): Unit = {
    order = 0
    probs.clear()
    marginals.clear()
  }

  /**
    * @return all symbols.
    */
  def getSymbols: mutable.Set[String] = {
    val s = mutable.Set.empty[String]
    for ((k, v) <- probs) {
      for (c <- k) {
        s += c
      }
    }
    s
  }

  /**
    * Checks whether a new marginal probability is consistent.
    * It must be between 0.0 and 1.0 and the sum of all marginals (including the new one) must be close to 1.0.
    *
    * @param nm The new marginal.
    * @return True if marginal is ok.
    */
  private def marginalConsistent(nm: Double): Boolean = {
    if (nm < 0.0 | nm > 1.0) return false
    val margSum = marginals.foldLeft(0.0)(_ + _._2)
    if ((margSum + nm) > 1.01) return false
    true
  }

  /**
    * Checks whether a new conditional is consistent.
    * It must be between 0.0 and 1.0 and the length of the conditional must be equal to the assumed order.
    *
    * @param s The symbol and the conditional.
    * @param p The probability.
    * @return True if new conditional is ok.
    */
  private def probConsistent(
                              s: List[String],
                              p: Double
                            ): Boolean = {
    if (probs.isEmpty) order = s.size - 1
    else if (order != s.size - 1) return false
    if (p < 0.0 | p > 1.0) return false
    true
  }

  def getOrder: Int = order

  override def toString: String = probs.toString() + "\n" + marginals.toString()
}
