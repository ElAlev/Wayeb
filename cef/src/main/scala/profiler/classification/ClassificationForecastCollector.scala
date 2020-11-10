package profiler.classification

import fsm.runtime.RunMessage
import model.forecaster.runtime.RelativeForecast
import profiler.ForecastCollector
import scala.collection.mutable.ArrayBuffer

/**
  * Collector for classification forecasts.
  * Collects all detections and forecasts and estimates true positives, etc., whenever
  * profiler.classification.ClassificationForecastCollector#getTotalCounts() is called.
  * Contrary to regression collectors, we cannot dump forecasts after every match. If we do this we won't be able to
  * properly estimate our metrics. E.g. imagine we have
  *
  * forecast:[10,20]  forecast:[4,8] detection:[7] detection:[17]
  *
  * If we dump our forecasts at detection 7, we will not be able to properly evaluate the first forecast as a true
  * positive at detection 17.
  * TODO: try to dump forecasts which have been evaluated with certainty, e.g., forecasts whose end point is smaller
  * than the current event counter. Then change also profiler.classification.ClassificationForecastCollector#reset().
  *
  * @param id The id of the pattern to which the collector belongs.
  */
case class ClassificationForecastCollector(id: Int) extends ForecastCollector {

  // for forecasts, we keep the forecast itself, the event timestamp and the value of the partition attribute
  protected var forecasts = new ArrayBuffer[(RelativeForecast, Long, String)]()

  // for detections, we keep the event counter and the value of the partition attribute
  protected var detections = new ArrayBuffer[(Long, String)]()

  /**
    * Collects forecasts and detections.
    *
    * @param rm The message sent from the FSM run to the predictor run.
    * @param forecast The forecast produced. Could be an empty one.
    */
  override def collect(
                        rm: RunMessage,
                        forecast: RelativeForecast
                      ): Unit = {
    if (rm.fmDetected) processNewDetection(rm)
    if (forecast.isValid) processNewForecast(rm, forecast)
  }

  /**
    * Processes a new detection. For each detection, we keep the event counter and the value of the partition attribute.
    *
    * @param rm The run message sent from the run.
    */
  private def processNewDetection(rm: RunMessage): Unit = {
    val newDetection = (rm.eventCounter, rm.attributeValue)
    detections += newDetection
  }

  /**
    * Processes a new forecast. For each forecast, we keep the forecast itself, the event timestamp and the value of the
    * partition attribute.
    *
    * @param rm The run message sent from the run.
    * @param forecast The new forecast.
    */
  private def processNewForecast(
                                  rm: RunMessage,
                                  forecast: RelativeForecast
                                ): Unit = {
    val toAdd = (forecast, rm.timestamp, rm.attributeValue)
    forecasts += toAdd
  }

  /**
    * Retrieves counts of true positives, true negatives, false positives, false negatives.
    *
    * @return (true positives, true negatives, false positives and false negatives)
    */
  def getTotalCounts: (Int, Int, Int, Int) = {
    // we first need to group by partition value
    val groupedDetections = detections.groupBy(_._2).mapValues(v => v.map(x => x._1))
    val groupedPredictions = forecasts.groupBy(_._3).mapValues(v => v.map(x => x._1))

    val gatheredCounts = groupedPredictions.map(x => {
      // isolate detections (their event counters)
      val theseDetections = if (groupedDetections.contains(x._1)) groupedDetections(x._1).toList else List.empty[Long]
      // isolate forecasts
      val theseForecasts = x._2.toList
      // count
      countPositivesNegatives(theseForecasts, theseDetections)
    }).toList

    // gather all counts from all partitions
    val totalCounts = gatheredCounts.foldLeft((0, 0, 0, 0)) {
      (acc, x) => (acc._1 + x._1, acc._2 + x._2, acc._3 + x._3, acc._4 + x._4)
    }
    totalCounts
  }

  /**
    * From a list of forecasts and a list of detections, we count true positives, true negatives, false positives and
    * false negatives.
    *
    * @param forecastsList The list of forecasts.
    * @param detectionsList The list of detections, as a list of event counters where each complex event occurred.
    * @return (true positives, true negatives, false positives and false negatives)
    */
  private def countPositivesNegatives(
                                       forecastsList: List[RelativeForecast],
                                       detectionsList: List[Long]
                                     ): (Int, Int, Int, Int) = {
    val evaluation = forecastsList.map(x => evaluateForecast(x, detectionsList))
    val tp = evaluation.count(x => x == 1)
    val tn = evaluation.count(x => x == -1)
    val fp = evaluation.count(x => x == 2)
    val fn = evaluation.count(x => x == -2)
    (tp, tn, fp, fn)
  }

  /**
    * Evaluates a forecast as true positive (1), true negative (-1), false positive (2) or false negative (-2) from a
    * list of detections.
    * For a forecast to be tp, it must be positive and there must exist at least one detection within the forecast's
    * interval.
    * For a forecast to be tn, it must be negative and there must exist no detection within the forecast's interval.
    * For a forecast to be fp, it must be positive and there must exist no detection within the forecast's interval.
    * For a forecast to be fn, it must be negative and there must exist at least one detection within the forecast's
    * interval.
    *
    * @param forecast The forecast.
    * @param detections The list of detections, as a list of event counters where each complex event occurred.
    * @return 1 for true positive, -1 for true negative, 2 for false positive, -2 for false negative.
    */
  private def evaluateForecast(
                                forecast: RelativeForecast,
                                detections: List[Long]
                              ): Int = {
    val start = forecast.startRelativeToCounter
    val end = forecast.endRelativeToCounter
    val isPositive = forecast.isPositive
    if (detections.exists(d => d >= start & d <= end)) {
      if (isPositive) 1 // true positive
      else -2 // false negative
    } else {
      if (isPositive) 2 // false positive
      else -1 // true negative
    }
  }

  /**
    * Do this when a RESET event arrives.
    * Do nothing here. We cannot dump forecasts since no statistics are estimated online. We need to keep everything
    * until the end so that profiler.classification.ClassificationForecastCollector#getTotalCounts() executes correctly.
    */
  def reset(): Unit = {
    //forecasts.clear()
    //detections.clear()
  }

}
