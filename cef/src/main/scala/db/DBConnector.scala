package db

import fsm.runtime.RunMessage
import model.forecaster.runtime.RelativeForecast
import slick.jdbc.PostgresProfile.api._
import ui.ConfigUtils
import utils.Shutdownable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

/**
  * A database connector that lets us connect to a database and store detected complex events and forecast intervals.
  * CAUTION: Writing operations are blocking. Enabling these operations has a severe performance impact.
  */
object DBConnector extends Shutdownable {
  val dbname: String = ConfigUtils.defaultDB
  private val forecasts = TableQuery[ForecastsTable]
  private val detections = TableQuery[DetectionsTable]
  private val db = Database.forConfig(dbname)
  private var PredId = 0
  private var DetId = 0
  private var myPredictions: List[(Long, Int, Boolean, Int, Double)] = List.empty

  // First drop tables if they exist
  // does not seem to work
  /*val drop = DBIO.seq(
    sqlu"DROP TABLE IF EXISTS #${forecasts.baseTableRow.tableName}",
    sqlu"DROP TABLE IF EXISTS #${detections.baseTableRow.tableName}"
  )
  val dropFuture = db.run(drop)
  Await.result(dropFuture, Duration.Inf)*/

  // Create the tables, including primary and foreign keys
  val setup = DBIO.seq(
    forecasts.schema.create,
    detections.schema.create
  )
  val setupFuture: Future[Unit] = db.run(setup)
  Await.result(setupFuture, Duration.Inf)

  /**
    * Write a new forecast interval to the forecasts table.
    *
    * @param forecast The forecast interval.
    * @param state The state the automaton was in when the forecast was emitted.
    * @param ts The timestamp of the forecast.
    * @param attributeVal The value of the partition attribute.
    * @param spread   The spread of the interval.
    * @param distance The distance of the interval from the current point (distance is defined as the conditional
    *                 expectation of the waiting-time distribution given the forecast interval),
    * @param isCorrect Whether the interval was correct.
    */
  def writeNewForecast(
                        forecast: RelativeForecast,
                        state: Int,
                        ts: Long,
                        attributeVal: String,
                        spread: Int,
                        distance: Double,
                        isCorrect: Boolean
                      ): Unit = {
    PredId += 1
    val writer = DBIO.seq(
      forecasts += (
        PredId,
        ts,
        attributeVal,
        state,
        forecast.startRelativeToNow,
        forecast.endRelativeToNow,
        forecast.startRelativeToCounter,
        forecast.endRelativeToCounter,
        forecast.prob,
        spread,
        distance,
        isCorrect
      )
    )
    val writerFuture = db.run(writer)
    Await.result(writerFuture, Duration.Inf)
  }

  /**
    * Writes a new complex event to the detections table.
    *
    * @param rm The message produced by an automaton run when it detects a complex event.
    */
  def writeNewDetection(rm: RunMessage): Unit = {
    DetId += 1
    val writer = DBIO.seq(
      detections += (
        DetId,
        rm.timestamp,
        rm.attributeValue,
        rm.currentState,
        rm.matchedEvents.toString()
      )
    )
    val writerFuture = db.run(writer)
    Await.result(writerFuture, Duration.Inf)
  }

  /**
    * Retrieves all forecasts stored in the database.
    *
    * @return A list of forecasts. Each forecast is a (timestamp,state,isCorrect,spread,distance) tuple.
    */
  def getForecasts: List[(Long, Int, Boolean, Int, Double)] = {
    myPredictions = List.empty
    val q1 = for (p <- forecasts) yield (p.ts, p.state, p.isCorrect, p.spread, p.distance)
    val readerFuture = db.run(q1.result).map(_.foreach(p =>
      myPredictions = (p._1, p._2, p._3, p._4, p._5) :: myPredictions)
    )
    //val readerFuture = db.run(forecasts.result)
    /*val readerFuture = db.run(forecasts.result).map(_.foreach {
      case (predID, ts, attr, state, startnow, endnow, startcount, endcount, prob, spread, dist, isCorrect) =>
        println("  " + ts + " " + isCorrect )
    })*/
    Await.result(readerFuture, Duration.Inf)
    myPredictions
  }

  def shutdown(): Unit = db.close()
}
