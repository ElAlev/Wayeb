package model.forecaster.runtime

import ui.ConfigUtils

object Forecast {
  /**
    * Constructs an invalid forecast.
    *
    * @return An invalid, dummy forecast.
    */
  def apply(): Forecast = new Forecast(-1, -1, -1.0, -1.0, false)

  /**
    * Constructs a non-classification forecast.
    *
    * @param start The interval's start point (greater than 0).
    * @param end The interval's end point (greater than 0 and greater or equal to start).
    * @param middle The middle of the interval.
    * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid).
    * @return The forecast interval.
    */
  def apply(
             start: Int,
             end: Int,
             middle: Double,
             prob: Double
           ): Forecast = {
    require(start > 0)
    require(end >= start)
    new Forecast(start, end, middle, prob, false)
  }

  /**
    * Constructs a forecast, either regression or classification.
    *
    * @param start The interval's start point (greater than 0).
    * @param end The interval's end point (greater than 0 and greater or equal to start).
    * @param middle The middle of the interval.
    * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid).
    * @param positive Used for classification forecasts. If true, this means that a complex event will be detected within
    *                 the interval. If false, this mean that it will not be detected.
    * @return The forecast interval.
    */
  def apply(
             start: Int,
             end: Int,
             middle: Double,
             prob: Double,
             positive: Boolean
           ): Forecast = {
    require(start > 0)
    require(end >= start)
    new Forecast(start, end, middle, prob, positive)
  }

}

/**
  * Class representing forecast intervals.
  *
  * @param start The interval's start point (greater than 0 for valid forecasts or -1 for invalid forecasts).
  * @param end The interval's end point (greater than 0 and greater or equal to start for valid forecasts or -1 for
  *            invalid forecasts).
  * @param middle The middle of the interval (>= start and <= end for valid, -1 for invalid forecasts).
  * @param prob The interval's probability (>= 0.0 and <= 1.0 for valid, -1.0 for invalid forecasts).
  * @param positive Used for classification forecasts. If true, this means that a complex event will be detected within
  *                 the interval. If false, this mean that it will not be detected.
  */
class Forecast private[runtime](
                                 val start: Int,
                                 val end: Int,
                                 val middle: Double,
                                 val prob: Double,
                                 val positive: Boolean
                               ) extends Serializable {

  require(end >= start)
  require(start > 0 | (start == -1 & end == -1 & middle == -1))
  require(
    middle >= start - ConfigUtils.intervalTolerance & middle <= end + ConfigUtils.intervalTolerance,
    "start,end,middle " + start + "," + end + "," + middle + " not valid"
  )
  require((prob >= -0.0001 & prob <= 1.0001) | (prob == -1.0 & start == -1), "Prob " + prob + " not valid")
  override def toString: String = start + "," + end + "," + middle + "," + prob + "," + positive

  def isValid: Boolean = start != -1
}
