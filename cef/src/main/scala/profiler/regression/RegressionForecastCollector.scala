package profiler.regression

import com.typesafe.scalalogging.LazyLogging
import db.DBConnector
import fsm.runtime.RunMessage
import model.forecaster.runtime.RelativeForecast
import profiler.ForecastCollector
import ui.ConfigUtils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Collector for regression forecasts.
  *
  * Regression collectors work a bit differently than the classification ones. With classification collectors, we
  * basically just collect forecasts during runtime and after forecasting is finished we can estimate statistics.
  * With regression collectors, we estimate some statistics during runtime. We only store forecasts until a new match is
  * detected. At that point, we retrieve all stored forecasts, evaluate them against the new match, update our
  * statistics and then we can dump these forecasts. We thus assume that each regression forecast target the next
  * complex event to occur and nothing else. When a reset event arrives, we need to to dump forecasts since we cannot
  * estimate some metrics for the current forecasts (e.g. the interval score) without knowing where the complex event
  * occurred. RESET events mean that the stream is essentially composed of substreams and we should not use any complex
  * events after a RESET to evaluate forecasts before it.
  *
  * @param id The id of the pattern to which the collector belongs.
  * @param states The set of FSM state ids. To be used for collection per state stats.
  */
case class RegressionForecastCollector(
                                        id: Int,
                                        states: Set[Int]
                                      ) extends ForecastCollector with LazyLogging {
  protected var forecasts = new ArrayBuffer[(RelativeForecast, Long, String)]()
  protected var forecastsNo = 0
  protected var correctForecastsNo = 0
  protected var totalSpread = 0
  protected var totalDistance: Double = 0.0
  protected var missed = 0
  protected var noForecasts = 0
  protected var allDetected = 0
  protected val forecastsPerState: mutable.Map[Int, ArrayBuffer[(RelativeForecast, Long, String)]] = mutable.Map[Int, ArrayBuffer[(RelativeForecast, Long, String)]]()
  protected val forecastsNoPerState: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  protected val correctForecastsNoPerState: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  protected val totalSpreadPerState: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  protected val totalDistancePerState: mutable.Map[Int, Double] = mutable.Map[Int, Double]()
  protected val missedPerState: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  protected var totalIntervalScore: Double = 0.0
  protected var totalConfidence: Double = 0.0
  protected var totalDeviation: Long = 0
  protected var totalDeviationSqr: Long = 0
  protected var totalRelativeDeviation: Double = 0.0

  initializeStats()

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
    * Processes a new detection. Increase the counter of detections, evaluate forecasts and dump them.
    *
    * @param rm The run message sent from the run.
    */
  private def processNewDetection(rm: RunMessage): Unit = {
    allDetected += 1
    evaluateForecasts(rm.eventCounter)
    evaluateForecastsPerState(rm.eventCounter)
    reset()
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
    addForecast2State(rm, forecast)
  }

  /**
    * Just initializes the per state structures.
    */
  private def initializeStats(): Unit = {
    for (i <- states) {
      forecastsNoPerState += (i -> 0)
      correctForecastsNoPerState += (i -> 0)
      missedPerState += (i -> 0)
      totalSpreadPerState += (i -> 0)
      totalDistancePerState += (i -> 0)
    }
  }

  /**
    * Stores the forecast to the structure responsible for the state from which it was emitted.
    *
    * @param rm The run message sent from the run.
    * @param forecast The new forecast.
    */
  def addForecast2State(
                         rm: RunMessage,
                         forecast: RelativeForecast
                       ): Unit = {
    val state = rm.currentState
    val toAdd = (forecast, rm.timestamp, rm.attributeValue)
    if (forecastsPerState.contains(state)) {
      forecastsPerState(state) += toAdd
    }
    else {
      val ab = new ArrayBuffer[(RelativeForecast, Long, String)]()
      ab += toAdd
      forecastsPerState += (state -> ab)
    }
  }

  /**
    * Evaluates a forecast against a detection.
    *
    * @param forecast The forecast.
    * @param detectionCounter The event counter of the detection.
    * @return True if the event counter is within the forecast interval.
    */
  private def evaluateForecast(
                                forecast: RelativeForecast,
                                detectionCounter: Long
                              ): Boolean =
    detectionCounter >= forecast.startRelativeToCounter & detectionCounter <= forecast.endRelativeToCounter

  /**
    * Evaluates all current forecasts against a detection.
    *
    * Interval score given by
    * NOIS = (uc − lc) + (lc − yc )Ix < lc + (yc − uc )Ix > uc
    *
    * @param detectionCounter The event counter of the detection.
    */
  private def evaluateForecasts(detectionCounter: Long): Unit = {
    var caught = false
    if (forecasts.isEmpty) noForecasts += 1
    for (forecast <- forecasts) {
      val relFore = forecast._1
      forecastsNo += 1
      totalSpread += (relFore.end - relFore.start)
      //totalDistance += relFore.middle
      totalIntervalScore += relFore.end - relFore.start
      totalConfidence += relFore.prob
      if (evaluateForecast(relFore, detectionCounter)) {
        correctForecastsNo += 1
        totalDistance += (detectionCounter - relFore.startRelativeToCounter)
        caught = true
      }
      else {
        // In order to avoid a possibly infinite score, set minimum alpha to 1 - 0.99
        val alpha = 1.0 - (if (relFore.prob == 1.0) 0.99 else relFore.prob)
        //val alpha = 1.0 - relFore.prob
        val deviation =
          if (detectionCounter < relFore.startRelativeToCounter) relFore.startRelativeToCounter - detectionCounter
          else detectionCounter - relFore.endRelativeToCounter
        totalDeviation += deviation
        totalDeviationSqr += deviation * deviation
        totalRelativeDeviation += deviation / detectionCounter.toDouble
        val thisScore = (2 / alpha) * deviation
        totalIntervalScore += thisScore
        if (relFore.prob == 1.0) {
          logger.debug("This/total score: " + thisScore + "/" + totalIntervalScore)
          logger.debug("Prob/Alpha: " + relFore.prob + "/" + alpha)
        }
      }
    }
    if (detectionCounter != -1 & !caught) missed += 1
  }

  /**
    * Evaluates all current forecasts against a detection on a per state basis.
    *
    * @param detectionCounter The event counter of the detection.
    */
  private def evaluateForecastsPerState(detectionCounter: Long): Unit = {
    for ((state, predsPerState) <- forecastsPerState) {
      var caught = false
      for (prediction <- predsPerState) {
        val relPred = prediction._1
        forecastsNoPerState(state) += 1
        val spread = relPred.end - relPred.start
        totalSpreadPerState(state) += spread
        val distance = relPred.middle
        if (evaluateForecast(relPred, detectionCounter)) {
          correctForecastsNoPerState(state) += 1
          totalDistancePerState(state) += (detectionCounter - relPred.startRelativeToCounter)
          caught = true
        }
        if (ConfigUtils.write2db) {
          val ts = prediction._2
          val attributeVal = prediction._3
          DBConnector.writeNewForecast(relPred, state, ts, attributeVal, spread, distance, caught)
        }
      }
      if (detectionCounter != -1 & !caught) missedPerState(state) += 1
    }
  }

  /**
    * For RESET events, dump all forecasts.
    */
  def reset(): Unit = {
    forecasts.clear()
    forecastsPerState.clear()
  }

  def getForecastsNo: Int = forecastsNo
  def getCorrectForecastsNo: Int = correctForecastsNo
  def getTotalSpread: Int = totalSpread
  def getTotalDistance: Double = totalDistance
  def getForecastsNoPerState: mutable.Map[Int, Int] = forecastsNoPerState
  def getCorrectForecastsNoPerState: mutable.Map[Int, Int] = correctForecastsNoPerState
  def getTotalSpreadPerState: mutable.Map[Int, Int] = totalSpreadPerState
  def getTotalDistancePerState: mutable.Map[Int, Double] = totalDistancePerState
  def getMissed: Int = missed
  def getMissedPerState: mutable.Map[Int, Int] = missedPerState
  def getDetectedNo: Int = allDetected
  def getTotalIntervalScore: Double = totalIntervalScore
  def getTotalConfidence: Double = totalConfidence
  def getTotalDeviation: Long = totalDeviation
  def getTotalDeviationSqr: Long = totalDeviationSqr
  def getTotalRelativeDeviation: Double = totalRelativeDeviation
  def getTotalNoForecasts: Int = noForecasts

}
