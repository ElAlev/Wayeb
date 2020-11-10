package profiler.regression

import java.io.FileWriter
import com.typesafe.scalalogging.LazyLogging
import profiler.StatsEstimator
import scala.collection.{SortedMap, mutable}

/**
  * Estimates various regression statistics from a list of forecast collectors.
  * CAUTION: Metrics that cannot be estimated (e.g., because the denominator in their formula is zero) have a value of
  * -1.0.
  *
  * @param collectors The collectors.
  */
class RegressionStatsEstimator(collectors: List[RegressionForecastCollector]) extends StatsEstimator with LazyLogging {
  private var predictionsNo = 0
  private var correctPredictionsNo = 0
  private var totalSpread = 0
  private var totalDistance: Double = 0.0
  // complex events for which no forecast was correct
  private var missed = 0
  // complex events for which no forecast was produced
  private var noForecasts = 0
  private var allDetected = 0
  private var precision = -1.0
  private var avgSpread = -1.0
  private var avgDist = -1.0
  // percentage of complex events which were not correctly predicted by any forecast
  private var lossRatio = -1.0
  // percentage of complex events for which no forecasts were produced
  private var noForecastsRatio = -1.0
  private var totalIntervalScore: Double = 0.0
  private var avgIntervalScore: Double = -1.0
  private var totalConfidence: Double = 0.0
  private var avgConfidence: Double = -1.0
  // deviation is (lc − yc )Ix < lc + (yc − uc )Ix > uc
  private var totalDeviation: Long = 0
  private var totalDeviationSqr: Long = 0
  private var totalRelativeDeviation: Double = 0.0
  private var avgDeviation: Double = -1.0
  private var rmse: Double = -1.0
  private var mae: Double = -1.0
  private var mare: Double = -1.0

  private val predictionsNoPerState = mutable.Map[Int, Int]()
  private val correctPredictionsNoPerState = mutable.Map[Int, Int]()
  private val totalSpreadPerState = mutable.Map[Int, Int]()
  private val totalDistancePerState = mutable.Map[Int, Double]()
  private val missedPerState = mutable.Map[Int, Int]()
  //private val noForecastsPerState = Map[Int,Int]()
  private val precisionPerState = mutable.Map[Int, Double]()
  private val avgSpreadPerState = mutable.Map[Int, Double]()
  private val avgDistPerState = mutable.Map[Int, Double]()
  private val lossRatioPerState = mutable.Map[Int, Double]()
  //private val noForecastsRatioPerState = Map[Int,Double]()

  /**
    * Compares those statistics against other statistics.
    *
    * @param otherMCStats The other statistics.
    * @return True if statistics are the same.
    */
  def compareAgainst(otherMCStats: RegressionStatsEstimator): Boolean = {
    if (predictionsNo != otherMCStats.getStat("predictionsNo").toInt) return false
    if (correctPredictionsNo != otherMCStats.getStat("correctPredictionsNo").toInt) return false
    if (totalSpread != otherMCStats.getStat("totalSpread").toInt) return false
    if (totalDistance != otherMCStats.getStat("totalDistance").toDouble) return false
    if (missed != otherMCStats.getStat("missed").toInt) return false
    if (allDetected != otherMCStats.getStat("allDetected").toInt) return false
    if (precision != otherMCStats.getStat("precision").toDouble) return false
    if (avgSpread != otherMCStats.getStat("avgSpread").toDouble) return false
    if (avgDist != otherMCStats.getStat("avgDist").toDouble) return false
    if (lossRatio != otherMCStats.getStat("lossRatio").toDouble) return false
    if (noForecastsRatio != otherMCStats.getStat("noForecastsRatio").toDouble) false
    else true
  }

  /**
    * Estimates statistics.
    */
  override def estimateStats(): Unit = {
    for (collector <- collectors) {
      correctPredictionsNo += collector.getCorrectForecastsNo
      predictionsNo += collector.getForecastsNo
      totalSpread += collector.getTotalSpread
      totalDistance += collector.getTotalDistance
      missed += collector.getMissed
      noForecasts += collector.getTotalNoForecasts
      allDetected += collector.getDetectedNo
      totalIntervalScore += collector.getTotalIntervalScore
      totalConfidence += collector.getTotalConfidence
      totalDeviation += collector.getTotalDeviation
      totalDeviationSqr += collector.getTotalDeviationSqr
      totalRelativeDeviation += collector.getTotalRelativeDeviation
    }
    if (predictionsNo != 0) {
      precision = correctPredictionsNo.toDouble / predictionsNo
      avgSpread = totalSpread.toDouble / predictionsNo.toDouble
      if (correctPredictionsNo != 0) avgDist = totalDistance / correctPredictionsNo.toDouble
      avgIntervalScore = totalIntervalScore / predictionsNo
      avgConfidence = totalConfidence / predictionsNo
      avgDeviation = totalDeviation.toDouble / predictionsNo
      rmse = math.sqrt(totalDeviationSqr.toDouble / predictionsNo)
      mae = totalDeviation.toDouble / predictionsNo
      mare = totalRelativeDeviation / predictionsNo
    }
    if (allDetected != 0) {
      lossRatio = missed.toDouble / allDetected.toDouble
      noForecastsRatio = noForecasts.toDouble / allDetected
    }
  }

  /**
    * Estimates per state statistics.
    */
  override def estimatePerStateStats(): Unit = {
    if (collectors.isEmpty) return
    var allDetectedV = 0
    for ((k, v) <- collectors.head.getForecastsNoPerState) {
      predictionsNoPerState += (k -> 0)
      correctPredictionsNoPerState += (k -> 0)
      missedPerState += (k -> 0)
      totalSpreadPerState += (k -> 0)
      totalDistancePerState += (k -> 0)
      precisionPerState += (k -> -1.0)
      avgSpreadPerState += (k -> -1.0)
      avgDistPerState += (k -> -1.0)
      lossRatioPerState += (k -> -1.0)
    }
    for (collector <- collectors) {
      val predictionsNoPerStateV: mutable.Map[Int, Int] = collector.getForecastsNoPerState
      val correctPredictionsNoPerStateV: mutable.Map[Int, Int] = collector.getCorrectForecastsNoPerState
      val totalSpreadPerStateV: mutable.Map[Int, Int] = collector.getTotalSpreadPerState
      val totalDistancePerStateV: mutable.Map[Int, Double] = collector.getTotalDistancePerState
      val missedPerStateV: mutable.Map[Int, Int] = collector.getMissedPerState
      allDetectedV += collector.getDetectedNo
      for ((k, _) <- predictionsNoPerStateV) {
        predictionsNoPerState(k) += predictionsNoPerStateV(k)
        correctPredictionsNoPerState(k) += correctPredictionsNoPerStateV(k)
        totalSpreadPerState(k) += totalSpreadPerStateV(k)
        totalDistancePerState(k) += totalDistancePerStateV(k)
        missedPerState(k) += missedPerStateV(k)
      }
    }
    for ((k, v) <- precisionPerState) {
      if (predictionsNoPerState(k) != 0) {
        precisionPerState(k) = correctPredictionsNoPerState(k).toDouble / predictionsNoPerState(k)
        lossRatioPerState(k) = missedPerState(k).toDouble / allDetectedV.toDouble
        avgSpreadPerState(k) = totalSpreadPerState(k).toDouble / predictionsNoPerState(k).toDouble
        if (correctPredictionsNoPerState(k) != 0)
          avgDistPerState(k) = totalDistancePerState(k) / correctPredictionsNoPerState(k).toDouble
      }
    }
  }

  /**
    * Retrieves the value for a certain metric.
    * Make sure stats have been estimated before trying to retrieve one. You must have called
    * profiler.classification.ClassificationPatternStats.estimateStats.
    *
    * @param which The metric:
    *              - correctPredictionsNo;
    *              - predictionsNo;
    *              - totalSpread;
    *              - totalDistance;
    *              - missed;
    *              - allDetected;
    *              - precision;
    *              - spread;
    *              - distance;
    *              - lossRatio;
    *              - noForecastsRatio;
    *              - interval;
    *              - confidence;
    *              - deviation;
    *              - rmse;
    *              - mae;
    *              - mare.
    *
    * @return The metric's value as a string.
    */
  override def getStat(which: String): String = {
    which match {
      case "correctPredictionsNo" => correctPredictionsNo.toString
      case "predictionsNo" => predictionsNo.toString
      case "totalSpread" => totalSpread.toString
      case "totalDistance" => totalDistance.toString
      case "missed" => missed.toString
      case "allDetected" => allDetected.toString
      case "precision" => precision.toString
      case "spread" => avgSpread.toString
      case "distance" => avgDist.toString
      case "lossRatio" => lossRatio.toString
      case "noForecastsRatio" => noForecastsRatio.toString
      case "interval" => avgIntervalScore.toString
      case "confidence" => avgConfidence.toString
      case "deviation" => avgDeviation.toString
      case "rmse" => rmse.toString
      case "mae" => mae.toString
      case "mare" => mare.toString
      case _ => throw new IllegalArgumentException("Stat not recognized: " + which)
    }
  }

  /**
    * @return Statistics as a comma separated row.
    */
  private def getStatsAsRow: String = {
    var row = ""
    row += predictionsNo.toString + "," + correctPredictionsNo.toString + "," + precision.toString
    row += "," + allDetected.toString + "," + missed.toString + "," + lossRatio.toString //+ "," + allDetectedGT.toString
    row += "," + avgSpread.toString + "," + avgDist.toString + "," + avgIntervalScore.toString
    row
  }

  /**
    * State statistics as a comma separated row.
    *
    * @param state The id of the state.
    * @return State statistics as a comma separated row.
    */
  private def getStatsPerStateAsRow(state: Int): String = {
    var row = ""
    row += state.toString
    row += "," + predictionsNoPerState(state).toString + "," + correctPredictionsNoPerState(state).toString + "," + precisionPerState(state).toString
    row += "," + allDetected.toString + "," + missedPerState(state).toString + "," + lossRatioPerState(state).toString //+ "," + allDetectedGT.toString
    row += "," + avgSpreadPerState(state).toString + "," + avgDistPerState(state).toString
    row
  }

  /**
    * Prints statistics and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  override def printProfileInfo(fn: String): Unit = {
    printProfileInfo()
    writeProfileInfo(fn)
  }

  /**
    * Prints statistics.
    */
  override def printProfileInfo(): Unit = {
    logger.info("\n***** FORECASTING STATS *****\n")
    logger.info("\tCorrect/Total Predictions: " + correctPredictionsNo + "/" + predictionsNo)
    logger.info("\tPrecision: " + precision)
    logger.info("\tMissed/Total Detections: " + missed + "/" + allDetected)
    logger.info("\tLoss ratio: " + lossRatio)
    logger.info("\tNo Forecasts/Total Detections: " + noForecasts + "/" + allDetected)
    logger.info("\tNo forecasts ratio: " + noForecastsRatio)
    logger.info("\tTotal/Avg spread: " + totalSpread + "/" + avgSpread)
    logger.info("\tTotal/Avg interval score: " + totalIntervalScore + "/" + avgIntervalScore)
    logger.info("\tAvg distance: " + avgDist)
    logger.info("\tRMSE/MAE/MARE: " + rmse + "/" + mae + "/" + mare)
  }

  /**
    * Writes statistics to a csv file.
    *
    * @param fn The path to the file.
    */
  def writeProfileInfo(fn: String): Unit = {
    val writer = new FileWriter(fn + ".csv", true)
    val row = getStatsAsRow
    writer.write(row + "\n")
    writer.close()
  }

  /**
    * Prints per state statistics and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  override def printPerStateProfileInfo(fn: String): Unit = {
    printPerStateProfileInfo()
    writePerStateProfileInfo(fn)
  }

  /**
    * Prints per state statistics.
    */
  override def printPerStateProfileInfo(): Unit = {
    val sorted = SortedMap(predictionsNoPerState.toSeq: _*)
    logger.info("\n***** STATS PER STATE *****")
    for ((k, _) <- sorted if precisionPerState(k) != -1.0) {
      logger.info("\n\t@STATE " + k)
      logger.info("\tCorrect/Total Predictions: " + correctPredictionsNoPerState(k) + "/" + predictionsNoPerState(k))
      logger.info("\tPrediction accuracy: " + precisionPerState(k))
      logger.info("\tMissed/Total Detections: " + missedPerState(k) + "/" + allDetected)
      logger.info("\tLoss ratio: " + lossRatioPerState(k))
      logger.info("\tAvg spread: " + avgSpreadPerState(k))
      logger.info("\tAvg distance: " + avgDistPerState(k))
    }
  }

  /**
    * Writes per state statistics to a csv file.
    *
    * @param fn The path to the file.
    */
  def writePerStateProfileInfo(fn: String): Unit = {
    val sorted = SortedMap(predictionsNoPerState.toSeq: _*)
    val writer = new FileWriter(fn + ".csv", true)
    for ((k, _) <- sorted) {
      val row = getStatsPerStateAsRow(k)
      writer.write(row + "\n")
    }
    writer.close()
  }

}
