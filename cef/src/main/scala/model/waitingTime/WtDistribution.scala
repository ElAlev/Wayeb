package model.waitingTime

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging
import model.forecaster.runtime.Forecast
import model.waitingTime.ForecastMethod.ForecastMethod
import ui.ConfigUtils

import scala.collection.SortedMap
import scala.util.control.Breaks._

object WtDistribution {

  /**
    * Constructor for waiting-time distribution.
    *
    * @param wt A map from indices (increasing by 1, starting from 1) to probabilities (>=0.0 & <=1.0)
    * @return A waiting-time distribution.
    */
  def apply(wt: Map[Int, Double]): WtDistribution = {
    //TODO: Check for possible underflows. Some very low values may be negative.
    val wt0 = wt.mapValues(v => if (v < 0.0) 0.0 else v)
    new WtDistribution(wt0)
  }

  /**
    * @return An empty waiting-time distribution.
    */
  def apply(): WtDistribution = new WtDistribution(Map.empty)
}

/**
  * Class representing and handling a waiting-time distribution.
  *
  * @param wt The distribution, as a map of indices and probabilities. Indices should be increasing, starting from 1 and
  *           end in horizon.
  */
class WtDistribution private (val wt: Map[Int, Double]) extends LazyLogging {

  /**
    * @return The total probability of the distribution. Normally, should be close to 1.0.
    */
  def sum: Double = wt.values.sum

  /**
    * @return True if the distributions has no entries, i.e., its horizon is 0.
    */
  def isEmpty: Boolean = wt.isEmpty

  /**
    * Estimates a forecast interval using the smartscan method.
    *
    * @param predictionThreshold The confidence threshold.
    * @param maxSpread The maximum spread.
    * @return The forecast interval.
    */
  def buildPrediction(
                       predictionThreshold: Double,
                       maxSpread: Int
                     ): Forecast = {
    buildForecast(ForecastMethod.SMARTSCAN, predictionThreshold, maxSpread)
  }

  /**
    * Estimates a forecast interval according to the method specified.
    *
    * @param center If the method is model.waitingTime.ForecastMethod#CLASSIFY_WIN(), then this will be the center of
    *               the window. See model.waitingTime.WtDistribution#getClassWinForecast(int, double, int).
    * @param method The method:
    *               model.waitingTime.ForecastMethod#ARGMAX(): Finds a forecast interval around the point with the
    *               maximum probability.
    *               See model.waitingTime.WtDistribution#getForecastArgMax(double, int).
    *
    *               model.waitingTime.ForecastMethod#FULLSCAN(): Estimates the best forecast interval by performing a
    *               full scan of the distribution.
    *               See model.waitingTime.WtDistribution#getForecastFullScan(double, int).
    *
    *               model.waitingTime.ForecastMethod#SMARTSCAN(): Estimates the best forecast interval by performing a
    *               single, efficient scan of the distribution.
    *               See model.waitingTime.WtDistribution#getForecastSmartScan(double, int).
    *
    *               model.waitingTime.ForecastMethod#FIXEDSPREAD(): Estimates the forecast interval of a given fixed
    *               spread with the highest probability. The spread is fixed here.
    *               See model.waitingTime.WtDistribution#getForecastFixedSpread(double, int).
    *
    *               model.waitingTime.ForecastMethod#CLASSIFY_NEXTK(): Estimates a classification forecast interval. The
    *               interval always starts at 1.
    *               See model.waitingTime.WtDistribution#getClassNextKForecast(double, int).
    *
    *               model.waitingTime.ForecastMethod#CLASSIFY_WIN(): Estimates a classification forecast interval. The
    *               interval is centered around center.
    *               See model.waitingTime.WtDistribution#getClassWinForecast(int, double, int).
    * @param confidenceThreshold The confidence threshold.
    * @param spread The spread, i.e., the interval length. Can have multiple interpretations depending on the method
    *               used.
    * @return A forecast interval.
    */
  def buildForecast(
                     center: Int,
                     method: ForecastMethod,
                     confidenceThreshold: Double,
                     spread: Int
                   ): Forecast = {
    require(confidenceThreshold >= 0.0 & confidenceThreshold <= 1.0)
    if (isEmpty) Forecast()
    else {
      require(spread >= 0 & spread <= wt.size, "maxSpread: " + spread + " greater than wt size: " + wt.size)
      method match {
        case ForecastMethod.ARGMAX => buildForecast(method, confidenceThreshold, spread)
        case ForecastMethod.FULLSCAN => buildForecast(method, confidenceThreshold, spread)
        case ForecastMethod.SMARTSCAN => buildForecast(method, confidenceThreshold, spread)
        case ForecastMethod.FIXEDSPREAD => buildForecast(method, confidenceThreshold, spread)
        case ForecastMethod.CLASSIFY_NEXTK => buildForecast(method, confidenceThreshold, spread)
        case ForecastMethod.CLASSIFY_WIN => getClassWinForecast(center, confidenceThreshold, spread)
      }
    }
  }

  /**
    * Same as model.waitingTime.WtDistribution#buildPrediction(int, scala.Enumeration.Value, double, int),
    * without the method model.waitingTime.ForecastMethod#CLASSIFY_WIN().
    *
    * @param method The method, besides model.waitingTime.ForecastMethod#CLASSIFY_WIN().
    *               See model.waitingTime.WtDistribution#buildPrediction(int, scala.Enumeration.Value, double, int).
    * @param confidenceThreshold The confidence threshold.
    * @param spread The spread.
    *               See model.waitingTime.WtDistribution#buildPrediction(int, scala.Enumeration.Value, double, int).
    * @return The forecast interval.
    */
  def buildForecast(
                     method: ForecastMethod,
                     confidenceThreshold: Double,
                     spread: Int
                   ): Forecast = {
    require(confidenceThreshold >= 0.0 & confidenceThreshold <= 1.0)
    if (isEmpty) Forecast()
    else {
      require(spread >= 0 & spread <= wt.size)
      method match {
        case ForecastMethod.ARGMAX => getForecastArgMax(confidenceThreshold, spread)
        case ForecastMethod.FULLSCAN => getForecastFullScan(confidenceThreshold, spread)
        case ForecastMethod.SMARTSCAN => getForecastSmartScan(confidenceThreshold, spread)
        case ForecastMethod.FIXEDSPREAD => getForecastFixedSpread(confidenceThreshold, spread)
        case ForecastMethod.CLASSIFY_NEXTK => getClassNextKForecast(confidenceThreshold, spread)
      }
    }
  }

  /**
    * Estimates a forecast interval. The interval is centered around a given point in the distribution.
    * This is in contrast to model.waitingTime.WtDistribution#getClassNextKForecast(double, int) which estimates
    * an interval always starting from 1.
    *
    * @param center The center around which the window is located.
    * @param confidenceThreshold The confidence threshold.
    * @param windowLength The window's length.
    * @return A positive forecast if the window's probability exceeds the threshold, otherwise a negative forecast.
    */
  private def getClassWinForecast(
                                   center: Int,
                                   confidenceThreshold: Double,
                                   windowLength: Int
                                 ): Forecast = {
    val leftTmp = center - (windowLength / 2)
    val left = if (leftTmp < 1) 1 else leftTmp
    val right = center + (windowLength / 2)
    val winProb = wt.filter(x => x._1 >= left & x._1 <= right).values.sum
    val positive = winProb >= confidenceThreshold
    Forecast(
      start    = left,
      end      = right,
      middle   = getConditionalExpectation(left, right),
      prob     = winProb,
      positive = positive
    )
  }

  /**
    * Estimates a classification forecast interval. Estimates the probability of the window starting from 1 and ending
    * at window and if it is greater than the threshold, the forecast is marked as positive.
    *
    * @param confidenceThreshold The confidence threshold.
    * @param windowLength The interval's window length.
    * @return A positive forecast if the window's probability exceeds the threshold, otherwise a negative forecast.
    */
  private def getClassNextKForecast(
                                     confidenceThreshold: Double,
                                     windowLength: Int
                                   ): Forecast = {
    val nextkProb = wt.filter(x => x._1 <= windowLength).values.sum
    val positive = nextkProb >= confidenceThreshold
    Forecast(
      start    = 1,
      end      = windowLength,
      middle   = getConditionalExpectation(1, windowLength),
      prob     = nextkProb,
      positive = positive
    )
  }

  /**
    * Estimates the forecast interval of a given fixed spread with the highest probability. The spread is fixed here.
    *
    * @param confidenceThreshold The confidence threshold.
    * @param fixedSpread The spread.
    * @return The forecast interval of spread fixedSpread with the highest probability that exceeds the threshold. Empty
    *         forecast if no such interval exists.
    */
  private def getForecastFixedSpread(
                                      confidenceThreshold: Double,
                                      fixedSpread: Int
                                    ): Forecast = {
    require(fixedSpread < wt.size)
    var startPointer = 1
    var endPointer = startPointer + fixedSpread
    var windowProb = getSliceProb(startPointer, endPointer)
    var start = -1
    var end = -1
    var prob = -1.0
    if (windowProb > confidenceThreshold) {
      start = startPointer
      end = endPointer
      prob = windowProb
    }
    while (endPointer < wt.size) {
      windowProb -= wt(startPointer)
      startPointer += 1
      endPointer += 1
      windowProb += wt(endPointer)
      if (windowProb > confidenceThreshold & windowProb > prob) {
        start = startPointer
        end = endPointer
        prob = windowProb
      }
    }
    if (start == -1) Forecast()
    else Forecast(start  = start, end = end, middle = getConditionalExpectation(start, end), prob = prob)
  }

  /**
    * Finds a forecast interval around the point with the maximum probability. First we find the index with the max prob
    * and then spread the interval to the left and right until we reach the confidence threshold.
    *
    *
    * @param confidenceThreshold The confidence threshold
    * @param maxSpread The maximum spread.
    * @return The forecast. Invalid interval if spread constraint cannot be satisfied.
    */
  private def getForecastArgMax(
                                 confidenceThreshold: Double,
                                 maxSpread: Int
                               ): Forecast = {
    var maxprob = 0.0
    var maxprobi = 0
    for ((k, v) <- wt) {
      if (v > maxprob) {
        maxprob = v
        maxprobi = k
      }
    }
    var probarea = maxprob
    var left = maxprobi
    var right = maxprobi
    var exhausted = false
    while (probarea < confidenceThreshold & !exhausted) {
      val newleft = left - 1
      val newright = right + 1
      if (wt.contains(newleft)) {
        probarea += wt(newleft)
        left = newleft
      }
      if (wt.contains(newright)) {
        probarea += wt(newright)
        right = newright
      }
      if (newleft != left & newright != right) exhausted = true
    }
    if (exhausted) {
      if (probarea < confidenceThreshold) logger.warn("Area lower than threshold")
    }
    if (maxSpread >= (right - left)) Forecast(start  = left, end = right, middle = maxprobi, prob = probarea)
    else Forecast()
  }

  /**
    * Implementation of the following algorithm for estimating a forecast interval.
    *
    * Estimating a forecast interval from a waiting-time distribution.
    * Input: A waiting-time distribution P with horizon h and a threshold θf c < 1.0
    * Output: The smallest interval I = (s, e) such that 1 ≤ s, e ≤ h, s ≤ e and P (I) ≥ θf c
    * s ← −1; e ← −1; i ← 1; j ← 1; p ← P (1);
    * while j 6= h do
    * /* Loop invariant: (s, e) is the smallest interval with P ((s, e)) > θf c among all
    * intervals with e ≤ j (or s = e = −1 in the first iteration). */
    * /* Expansion phase.*/
    * while (p < θf c ) ∧ (j < h) do
    *   j ← j + 1;
    *   p ← p + P (j);
    * /* Shrinking phase.*/
    * while p ≥ θf c do
    *   i ← i + 1;
    *   p ← p − P (i);
    * i ← i − 1;
    * /* s = −1 indicates that no interval has been found yet, i.e., that this is the
    * first iteration.*/
    * if (spread((i, j)) < spread((s, e))) ∨ (s = −1) then
    *   s ← i;
    *   e ← j;
    * return (s, e);
    *
    * @param confidenceThreshold The confidence threshold
    * @param maxSpread The maximum spread.
    * @return The best forecast interval or an invalid forecast if threshold and spread constraints cannot be satisfied.
    */
  private def getForecastSmartScan(
                                    confidenceThreshold: Double,
                                    maxSpread: Int
                                  ): Forecast = {
    val horizon = wt.size
    var interval: List[Int] = List(1, horizon)
    var distributionExhausted = false

    var start = 0
    var end = 0
    var currentProb = 0.0
    //var newStart = 0
    //var newStartJump = 0
    breakable {
      while (!distributionExhausted) {
        val currentSpread = interval(1) - interval(0)
        val mf = expand(start + 1, confidenceThreshold)
        end = mf._1
        var thisProb = mf._2
        if (end == horizon) distributionExhausted = true
        if (end == 0) break
        //TODO: no need to set pointer at start+1. Can set it at end-currentSpread (but optimization not effective)
        val ru = shrink(start + 1, end, thisProb, currentSpread, confidenceThreshold)
        start = ru._1
        thisProb = ru._2
        val thisSpread = end - start
        if (thisSpread < currentSpread) {
          interval = List(start, end)
          currentProb = thisProb
        } else if (thisSpread == currentSpread) {
          if (thisProb > currentProb) {
            interval = List(start, end)
            currentProb = thisProb
          }
        }
      }
    }
    if ((interval(1) - interval(0)) <= maxSpread) Forecast(start  = interval(0), end = interval(1), middle = getConditionalExpectation(interval(0), interval(1)), prob = currentProb)
    else Forecast()
  }

  /**
    * The shrinking phase of model.waitingTime.WtDistribution#getForecastSmartScan(double, int).
    *
    * @param start Current start.
    * @param end Current end.
    * @param initProb Current probability.
    * @param currentSpread Current spread.
    * @param confidenceThreshold The prediction thershold.
    * @return A new shrunk interval with its probability.
    */
  private def shrink(
                      start: Int,
                      end: Int,
                      initProb: Double,
                      currentSpread: Int,
                      confidenceThreshold: Double
                    ): (Int, Double) = {
    var result = (0, 0.0)
    var newProb = initProb
    var breakFound = false
    var newStart = start
    /* this optimization does not seem effective
    val startJump = end - currentSpread
    if (startJump > newStart) {
      for (i<- newStart to startJump-1) {
        newProb -= wt(i)
      }
      newStart = startJump
    }*/
    while (!breakFound & newStart != end) {
      val candidateNewStart = newStart + 1
      val candidateNewProb = newProb - wt(newStart)
      if (candidateNewProb < confidenceThreshold) {
        breakFound = true
      } else {
        newStart = candidateNewStart
        newProb = candidateNewProb
      }
    }
    result = (newStart, newProb)
    result
  }

  /**
    * The expansion phase of model.waitingTime.WtDistribution#getForecastSmartScan(double, int).
    *
    * @param start Current start.
    * @param confidenceThreshold The confidence threshold.
    * @return The new expanded interval with its probability.
    */
  private def expand(
                      start: Int,
                      confidenceThreshold: Double
                    ): (Int, Double) = {
    val horizon = wt.size
    var breakFound = false
    var spread = 0
    var prob = 0.0
    var result = (0, 0.0)
    var end = 0
    while (!breakFound & end < horizon) {
      end = start + spread
      prob += wt(end)
      if (prob >= confidenceThreshold) {
        breakFound = true
        result = (end, prob)
      }
      spread += 1
    }
    result
  }

  /**
    * Estimates the best forecast interval (one that exceeds the confidence threshold and has the smallest spread below
    * max spread) by performing a full scan of the distribution. Inefficient. Used only for testing.
    *
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The max spread.
    * @return The best forecast interval or an invalid forecast if threshold and spread constraints cannot be satisfied.
    */
  private def getForecastFullScan(
                                   confidenceThreshold: Double,
                                   maxSpread: Int
                                 ): Forecast = {
    var spread = 0
    val horizon = wt.size
    var foundInterval = false
    var interval = (0, 0, 0.0, 0.0) //List(0,0)
    while (!foundInterval & spread <= horizon) {
      spread += 1
      interval = findBestInterval(spread, confidenceThreshold)
      if (interval._1 != 0) {
        foundInterval = true
      }
    }
    if (foundInterval & interval._2 - interval._1 <= maxSpread) Forecast(start  = interval._1, end = interval._2, middle = interval._3, prob = interval._4)
    else Forecast()
  }

  /**
    * This is where the actual scan takes place from model.waitingTime.WtDistribution#getForecastFullScan(double, int).
    *
    * @param spread The max spread.
    * @param predictionThreshold The confidence threshold.
    * @return The best interval found. (0,0) if none found.
    */
  private def findBestInterval(
                                spread: Int,
                                predictionThreshold: Double
                              ): (Int, Int, Double, Double) = {
    val horizon = wt.size
    var bestInterval = List(0, 0)
    var highestProb = 0.0
    for (s <- 1 to horizon - spread + 1) {
      val e = s + spread - 1
      val sliceProb = getSliceProb(s, e)
      if (sliceProb >= predictionThreshold & sliceProb > highestProb) {
        bestInterval = List(s, e)
        highestProb = sliceProb
      }
    }
    (bestInterval(0), bestInterval(1), getConditionalExpectation(bestInterval(0), bestInterval(1)), highestProb)
  }

  /**
    * @return The distribution's expected value.
    */
  def getExpectedValue: Double = {
    if (isEmpty) -1.0
    else wt.map(x => x._1 * x._2).sum
  }

  /**
    * Estimates the conditional expectation of the distribution, given an interval.
    *
    * @param left The given interval's left limit.
    * @param right The given interval's right limit.
    * @return The conditional expected value.
    */
  private def getConditionalExpectation(
                                         left: Int,
                                         right: Int
                                       ): Double = {
    if (left == 0) 0.0
    else {
      val intervalProb = (left to right).map(x => wt(x)).sum
      if (intervalProb > 0) (left to right).map(x => x * wt(x)).sum / intervalProb
      else (left + right) / 2.0
    }
  }

  /**
    * Estimates the probability of a slice/interval of the distribution.
    *
    * @param start The interval's left limit.
    * @param end The interval's right limit.
    * @return The interval's probability.
    */
  private def getSliceProb(
                            start: Int,
                            end: Int
                          ): Double = {
    var sliceProb = 0.0
    for (i <- start to end) {
      sliceProb += wt(i)
    }
    sliceProb
  }

  /**
    * @return The distribution sorted by its indices.
    */
  def getDistributionSorted: SortedMap[Int, Double] = {
    val sorted = SortedMap(wt.toSeq: _*)
    sorted
  }

  /**
    * @return The distribution as two vector, one for the indices, one for the probability values.
    */
  def getDistributionVectors: (DenseVector[Int], DenseVector[Double]) = {
    val ds = getDistributionSorted
    val timeVector = new DenseVector[Int](wt.size)
    val probVector = new DenseVector[Double](wt.size)
    var i = 0
    for ((k, v) <- ds) {
      timeVector(i) = k
      probVector(i) = v
      i += 1
    }
    (timeVector, probVector)
  }

  /**
    * Compares the distribution against another one. They must have the same domain and their respective probabilities
    * must be "close" to equal.
    *
    * @param that The other waiting-time distribution.
    * @return True if the two distributions are equal.
    */
  def compareAgainst(that: WtDistribution): Boolean = {
    val thisIndices = this.wt.keySet
    val thatIndices = that.wt.keySet
    if (thisIndices != thatIndices) false
    else {
      thisIndices.forall(i => {
        val thisProb = this.wt(i)
        val thatProb = that.wt(i)
        math.abs(thisProb - thatProb) < ConfigUtils.wtDistTolerance
      })
    }
  }

  override def toString: String = getDistributionSorted.toString()

}
