package model.forecaster.runtime

object RelativeForecast {
  /**
    * Constructor for dummy, invalid forecasts.
    * @return
    */
  def apply(): RelativeForecast = new RelativeForecast(-1, -1, -1, -1.0, -1, -1, false)

  /**
    * Constructs a non-classification forecast.
    *
    * @param startRelativeToNow The interval's start point (greater than 0).
    * @param endRelativeToNow The interval's end point (greater than 0 and greater or equal to start).
    * @param middle The middle of the interval.
    * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid).
    * @param startRelativeToCounter The interval's start point, but relative to the counter of the event that triggered
    *                               the forecast (greater than 0 for valid forecasts or -1 for invalid forecasts).
    * @param endRelativeToCounter The interval's end point , but relative to the counter of the event that triggered
    *                             the forecast (greater than 0 and greater or equal to startRelativeToCounter for valid
    *                             forecasts or -1 for invalid forecasts).
    * @return The forecast.
    */
  def apply(
             startRelativeToNow: Int,
             endRelativeToNow: Int,
             middle: Double,
             prob: Double,
             startRelativeToCounter: Long,
             endRelativeToCounter: Long
           ): RelativeForecast = {
    if (startRelativeToNow == -1) RelativeForecast()
    else new RelativeForecast(
      startRelativeToNow,
      endRelativeToNow,
      middle,
      prob,
      startRelativeToCounter,
      endRelativeToCounter,
      false
    )
  }

  /**
    * Constructs a forecast, either regression or classification.
    *
    * @param startRelativeToNow The interval's start point (greater than 0).
    * @param endRelativeToNow The interval's end point (greater than 0 and greater or equal to start).
    * @param middle The middle of the interval.
    * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid).
    * @param startRelativeToCounter The interval's start point, but relative to the counter of the event that triggered
    *                               the forecast (greater than 0 for valid forecasts or -1 for invalid forecasts).
    * @param endRelativeToCounter The interval's end point , but relative to the counter of the event that triggered
    *                             the forecast (greater than 0 and greater or equal to startRelativeToCounter for valid
    *                             forecasts or -1 for invalid forecasts).
    * @param isPositive Used for classification forecasts. If true, this means that a complex event will be detected within
    *                   the interval. If false, this mean that it will not be detected.
    * @return The forecast.
    */
  def apply(
             startRelativeToNow: Int,
             endRelativeToNow: Int,
             middle: Double,
             prob: Double,
             startRelativeToCounter: Long,
             endRelativeToCounter: Long,
             isPositive: Boolean
           ): RelativeForecast = {
    if (startRelativeToNow == -1) RelativeForecast()
    else new RelativeForecast(
      startRelativeToNow,
      endRelativeToNow,
      middle,
      prob,
      startRelativeToCounter,
      endRelativeToCounter,
      isPositive
    )
  }
}

/**
  * Class for representing forecast intervals. Similar to model.predictor.runtime.Prediction, but the interval's start
  * and end points are also given relative to the event counter. We need this extra information in order to be able to
  * evaluate a forecast. When we actually detect a complex event, we use startRelativeToCounter, endRelativeToCounter
  * and the complex events counter to determine whether the forecast was correct
  * (correct if CEcounter >= startRelativeToCounter and CEcounter <= endRelativeToCounter).
  *
  * @param startRelativeToNow The interval's start point (greater than 0 for valid forecasts or -1 for invalid forecasts).
  * @param endRelativeToNow   The interval's end point (greater than 0 and greater or equal to start for valid forecasts
  *                           or -1 for invalid forecasts).
  * @param middle The middle of the interval (>= start and <= end for valid, -1 for invalid forecasts).
  * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid, -1.0 for invalid forecasts).
  * @param startRelativeToCounter The interval's start point, but relative to the counter of the event that triggered
  *                               the forecast (greater than 0 for valid forecasts or -1 for invalid forecasts).
  * @param endRelativeToCounter The interval's end point , but relative to the counter of the event that triggered
  *                             the forecast (greater than 0 and greater or equal to startRelativeToCounter for valid
  *                             forecasts or -1 for invalid forecasts).
  * @param isPositive Used for classification forecasts. If true, this means that a complex event will be detected within
  *                   the interval. If false, this mean that it will not be detected.
  */
class RelativeForecast private(
                                val startRelativeToNow: Int,
                                val endRelativeToNow: Int,
                                middle: Double,
                                prob: Double,
                                val startRelativeToCounter: Long,
                                val endRelativeToCounter: Long,
                                val isPositive: Boolean
                              ) extends Forecast(startRelativeToNow, endRelativeToNow, middle, prob, isPositive) {

  require(startRelativeToCounter > 0 | startRelativeToCounter == -1, "startRelativeToCounter " + startRelativeToCounter + " not valid")
  require(endRelativeToCounter >= startRelativeToCounter)

  override def toString: String =
    "StartRelativeToNow->" + startRelativeToNow +
      "|EndRelativeToNow->" + endRelativeToNow +
      "|Middle->" + middle +
      "|Prob->" + prob +
      "|StartRelativeToCounter->" + startRelativeToCounter +
      "|EndRelativeToCounter->" + endRelativeToCounter +
      "|Positive->" + isPositive
}
