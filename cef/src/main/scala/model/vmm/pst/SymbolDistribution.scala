package model.vmm.pst

import model.vmm.Symbol
import ui.ConfigUtils

import scala.collection.SortedMap

object SymbolDistribution {
  /**
    * Constructor for distribution.
    *
    * @param dist The distribution map.
    * @return The distribution.
    */
  def apply(dist: Map[Symbol, Double]): SymbolDistribution = new SymbolDistribution(dist)

  /**
    * Constructor for an empty distribution.
    *
    * @return An empty distribution.
    */
  def apply(): SymbolDistribution = new SymbolDistribution(Map.empty[Symbol, Double])
}

/**
  * Class representing a next symbol distribution. For each possible next symbol, a probability value.
  *
  * @param dist A map of symbols to probability values.
  */
class SymbolDistribution(val dist: Map[Symbol, Double]) extends Serializable {
  private val tolerance = ConfigUtils.symbolDistTolerance
  require(dist.values.forall(p => p >= 0 & p <= 1.0), "Not valid probability values " + dist.values)
  require(sumsTo1, "Distribution sums to " + dist.values.sum + " must sum to 1 plus/minus " + tolerance + "Dist: " + dist)
  //private val  = d

  /**
    * Compares this distribution to another one. For the distributions to be equal, they must have the same symbols, and
    * their probability values must not differ (in absolute value) more than the given margin.
    *
    * @param other The other distribution.
    * @param margin The given margin.
    * @return True if the two distributions are "equal".
    */
  def compare(
               other: SymbolDistribution,
               margin: Double
             ): Boolean = {
    require(margin >= 0.0 & margin <= 0.1)
    if (this.dist.keySet != other.dist.keySet) false
    else {
      this.dist.forall(x => {
        val thatprob = other.getProbFor(x._1)
        val thisProb = x._2
        val probDiff = math.abs(thisProb - thatprob)
        probDiff <= margin
      })
    }
  }

  /**
    * Retrieves the probability of a given symbol (which must exist).
    *
    * @param symbol The given symbol.
    * @return The symbol's probability.
    */
  def getProbFor(symbol: Symbol): Double = {
    require(dist.contains(symbol), "Distribution does not contain " + symbol)
    dist(symbol)
  }

  /**
    * Smooths the distribution. All symbols with zero probability are assigned a minimum probability and the
    * distribution is re-normalized.
    *
    * @param gammaMin The minimum probability assigned to symbols with previously zero probabilities.
    * @param allSymbols All symbols that should be present in the distribution. Symbols not already present are assumed
    *                   to have zero probability.
    * @return The smoothed distribution.
    */
  def smooth(
              gammaMin: Double,
              allSymbols: Set[Symbol]
            ): SymbolDistribution = {
    require(gammaMin > 0 & gammaMin < 1.0)
    val missingSymbols = allSymbols &~ dist.keySet
    val missingDist = missingSymbols.map(x => x -> 0.0).toMap
    val distWithMissing = dist ++ missingDist
    val smoothed = smooth(distWithMissing, gammaMin)
    SymbolDistribution(smoothed)
  }

  /**
    * Smooths a given distribution. All symbols with zero probability are assigned a minimum probability and the
    * distribution is re-normalized.
    *
    * @param dist The given distribution to be smoothed.
    * @param gammaMin The minimum probability assigned to symbols with previously zero probabilities.
    * @return The smoothed distribution.
    */
  private def smooth(
                      dist: Map[Symbol, Double],
                      gammaMin: Double
                    ): Map[Symbol, Double] = {
    val symbolsNo = dist.size
    val smoothed = dist.map(x => (x._1, x._2 * (1 - symbolsNo * gammaMin) + gammaMin))
    smoothed
  }

  /**
    * Same as model.vmm.pst.SymbolDistribution#smooth(double, scala.collection.immutable.Set), but allows distribution
    * to be serialized.
    *
    * Smooths the distribution. All symbols with zero probability are assigned a minimum probability and the
    * distribution is re-normalized.
    *
    * @param gammaMin The minimum probability assigned to symbols with previously zero probabilities.
    * @param allSymbols All symbols that should be present in the distribution. Symbols not already present are assumed
    *                   to have zero probability.
    * @return The smoothed distribution.
    */
  def smoothSer(
                 gammaMin: Double,
                 allSymbols: Set[Symbol]
               ): SymbolDistribution = {
    require(gammaMin >= 0 & gammaMin < 1.0)
    val missingSymbols = allSymbols &~ dist.keySet
    val missingDist = missingSymbols.map(x => x -> 0.0).toMap
    val distWithMissing = dist ++ missingDist
    val smoothed = smoothSer(distWithMissing, gammaMin)
    SymbolDistribution(smoothed)
  }

  /**
    * Same as model.vmm.pst.SymbolDistribution#smooth(double, scala.collection.immutable.Set), but allows distribution
    * to be serialized.
    *
    * Smooths a given distribution. All symbols with zero probability are assigned a minimum probability and the
    * distribution is re-normalized.
    *
    * @param dist The given distribution to be smoothed.
    * @param gammaMin The minimum probability assigned to symbols with previously zero probabilities.
    * @return The smoothed distribution.
    */
  private def smoothSer(
                         dist: Map[Symbol, Double],
                         gammaMin: Double
                       ): Map[Symbol, Double] = {
    val factor = 1 - dist.size * gammaMin
    // .map(identity) used to make class serializable (otherwise mapValues prevents serialization)
    val smoothed = dist.mapValues(v => v * factor + gammaMin).map(identity)
    smoothed
  }

  /**
    * Checks whether the distribution sums to 1.0, with some tolerance.
    *
    * @return True if the sum of the distribution is "close" to 1.0.
    */
  private def sumsTo1: Boolean = {
    val sum = dist.values.sum
    val lowerBound = 1.0 - tolerance
    val upperBound = 1.0 + tolerance
    sum >= lowerBound & sum <= upperBound
  }

  /**
    * @return True if the distribution is consistent, i.e., its sum is close to 1.0.
    */
  def isConsistent: Boolean = sumsTo1

  /**
    * @return The distribution sum.
    */
  def sum: Double = dist.values.sum

  /**
    * Alternative stringification retaining only symbols with probabilities above a given threshold.
    *
    * @param threshold The given threshold.
    * @return The distribution string without low probability symbols.
    */
  def toString(threshold: Double): String = dist.filter(_._2 >= threshold).toString()

  override def toString: String = {
    val sorted = SortedMap(dist.toSeq: _*)
    sorted.toString()
  }
}
