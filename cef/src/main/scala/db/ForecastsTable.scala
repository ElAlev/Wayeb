package db

import slick.jdbc.PostgresProfile.api._
import ui.ConfigUtils

/**
  * Table to store emitted forecasts.
  * For each forecast, we store
  *   its timestamp,
  *   the value of its partition attribute,
  *   the state the automaton was in when the forecast was emitted,
  *   the start limit of the forecast interval, relative to the point when the forecast was produced
  *     (startRelativeToNow > 0),
  *   the end limit of the forecast interval, relative to the point when the forecast was produced,
  *   the start limit of the forecast interval, relative to the point of the previously detected complex event
  *     (startRelativeToCounter > counter), where counter is the number of events elapsed from the previously detected
  *     complex event,
  *   the end limit of the forecast interval, relative to the point of the previously detected complex event,
  *   the probability of the forecast interval,
  *   the spread of the interval,
  *   the distance of the forecast interval from the current point (distance is defined as the conditional expectation
  *     of the waiting-time distribution given the forecast interval),
  *   whether the forecast interval was correct.
  *
  * @param tag
  */
class ForecastsTable(tag: Tag) extends Table[(Int, Long, String, Int, Int, Int, Long, Long, Double, Int, Double, Boolean)](tag, Some(ConfigUtils.forecastsSchema), ConfigUtils.forecastsTable) {
  def id = column[Int]("PRED_ID", O.PrimaryKey) // This is the primary key column
  def ts = column[Long]("ts")
  def attr = column[String]("partitionval")
  def state = column[Int]("state")
  //def detection = column[Int]("detectionTime")
  def startRelativeToNow = column[Int]("startRelativeToNow")
  def endRelativeToNow = column[Int]("endRelativeToNow")
  def startRelativeToCounter = column[Long]("startRelativeToCounter")
  def endRelativeToCounter = column[Long]("endRelativeToCounter")
  def prob = column[Double]("prob")
  def spread = column[Int]("spread")
  def distance = column[Double]("distance")
  def isCorrect = column[Boolean]("isCorrect")

  def * = (id, ts, attr, state, startRelativeToNow, endRelativeToNow, startRelativeToCounter, endRelativeToCounter, prob, spread, distance, isCorrect)
}
