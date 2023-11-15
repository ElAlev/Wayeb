package profiler

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer

object NextProfiler {
  /**
    * Constructor for next profiler.
    *
    * @param streamSize The size of the test stream.
    * @return The next profiler.
    */
  def apply(streamSize: Int): NextProfiler = new NextProfiler(streamSize)
}

/**
  * Profiler for experiments predicting the next top k.
  *
  * @param streamSize The size of the test stream.
  */
class NextProfiler(streamSize: Int) extends ProfilerInterface with LazyLogging {
  private var execTime: Long = 0
  private var precision: Double = -1.0 // precision of -1.0 means that no forecasts were actually produced
  private var forecastsNo = 0
  private var correctForecastsNo = 0
  private var matchesNo = 0
  private val forecasts: ArrayBuffer[List[(Int, Double)]] = ArrayBuffer.empty
  private val detections: ArrayBuffer[List[Boolean]] = ArrayBuffer.empty

  /**
    * Only precision needs to be estimated.
    */
  override def estimateStats(): Unit = estimatePrecision()

  /**
    * Sets the total execution time.
    *
    * @param et The total execution time.
    */
  def setExecTime(et: Long): Unit = execTime = et

  /**
    * Adds a new forecast, i.e., a new list with the top k most probable next detections.
    *
    * @param forecast The new forecast, as list of pattern ids and their probabilities.
    */
  def addNewForecast(forecast: List[(Int, Double)]): Unit = forecasts += forecast

  /**
    * Adds a new "detection", i.e., a list with each pattern's decision about whether the respective complex event was
    * detected or not.
    *
    * @param detection The list with each pattern's decision.
    */
  def addNewDetection(detection: List[Boolean]): Unit = {
    detections += detection
    matchesNo += detection.count(_ == true)
  }

  /**
    * Prints all statistics.
    */
  def printProfileInfo(): Unit = {
    val throughput = (streamSize.toDouble / execTime.toDouble) * 1000000000
    logger.info("\n***** STATS *****")
    logger.info("\tStream processed in " + (execTime / 1000000) + "ms")
    logger.info("\tThroughput : " + throughput + " events/sec")
    logger.info("\tExecution time: " + (execTime / 1000000) + "ms")
    logger.info("\tMatches No: " + matchesNo)
    logger.info("\tPrediction accuracy: " + "%.3f".format(precision))
    logger.info("\tCorrect/Total Predictions: " + correctForecastsNo + "/" + forecastsNo)
  }

  /**
    * Prints all calculated statistics and also writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  def printProfileInfo(fn: String): Unit = {
    printProfileInfo()
    writeProfileInfo(fn)
  }

  /**
    * Prints all calculated statistics and also writes them to a csv file. Additionally, the prefix will be written in
    * the first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  def printProfileInfo(
                        prefix: String,
                        fn: String
                      ): Unit = {
    printProfileInfo()
    writeProfileInfo(prefix, fn)
  }

  /**
    * Estimates the precision of the forecasts. From the top k predicted complex events, all of them must have occurred
    * at the next timepoint for a forecast to be considered as correct.
    *
    * @return The precision.
    */
  private def estimatePrecision(): Double = {
    require(forecasts.size == detections.size)
    // for each timepoint, we find which complex events were actually detected
    // we find the indices of the patterns which were true
    val trueIndices = detections.map(d => d.zipWithIndex.filter(_._1 == true).map(e => e._2))
    // now build a similar buffer with the predicted indices
    val preds = forecasts.map(l => l.map(e => e._1))
    // and zip those two
    val tpz = trueIndices.zip(preds)
    // mark a forecast as true if all its predicted patterns/indices were included in the respective detection
    val tp = tpz.map(ll => ll._1.nonEmpty & ll._1.forall(l1 => ll._2.contains(l1))).toList
    val correctForecasts = tp.filter(_ == true)
    correctForecastsNo = correctForecasts.size
    forecastsNo = forecasts.size
    precision = correctForecastsNo.toDouble / forecastsNo
    precision
  }

  /**
    * Writes statistics to a csv file.
    *
    * @param fn The path to the file.
    */
  private def writeProfileInfo(fn: String): Unit = {
    val writer = CSVWriter.open(fn, append = true)
    val row = List(
      streamSize.toString,
      execTime.toString,
      matchesNo.toString,
      forecastsNo.toString,
      correctForecastsNo.toString,
      precision.toString
    )
    writer.writeRow(row)
    writer.close()
  }

  /**
    * Writes statistics to a csv file and adds the prefix to the first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  private def writeProfileInfo(
                                prefix: String,
                                fn: String
                              ): Unit = {
    val writer = CSVWriter.open(fn, append = true)
    val row = List(
      prefix,
      streamSize.toString,
      execTime.toString,
      matchesNo.toString,
      forecastsNo.toString,
      correctForecastsNo.toString,
      precision.toString
    )
    writer.writeRow(row)
    writer.close()
  }

}
