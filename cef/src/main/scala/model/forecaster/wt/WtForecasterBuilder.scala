package model.forecaster.wt

import java.io.FileWriter

import com.typesafe.scalalogging.LazyLogging
import fsm.FSMInterface
import model.waitingTime.ForecastMethod.ForecastMethod
import model.forecaster.ForecasterType
import model.forecaster.runtime.Forecast
import model.waitingTime.{ForecastMethod, WtDistribution}
import ui.ConfigUtils

import scala.collection.SortedMap

object WtForecasterBuilder extends LazyLogging {
  /**
    * Constructor for wt predictor builder. Uses the default forecast method.
    *
    * @param fsm The FSM for which we want to estimate the forecast intervals.
    * @param wtd The waiting-time distributions.
    * @param horizon The horizon.
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The max spread.
    * @return The wt predictor builder.
    */
  def apply(
             fsm: FSMInterface,
             wtd: Map[Int, WtDistribution],
             horizon: Int,
             confidenceThreshold: Double,
             maxSpread: Int
           ): WtForecasterBuilder = {
    WtForecasterBuilder(fsm, wtd, horizon, confidenceThreshold, maxSpread, ConfigUtils.defaultForeMethod)
  }

  /**
    * Constructor for wt predictor builder.
    *
    * @param fsm The FSM for which we want to estimate the forecast intervals.
    * @param wtd The waiting-time distributions.
    * @param horizon The horizon.
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The max spread.
    * @param method The forecast method, REGRESSION or CLASSIFICATION.
    * @return The wt predictor builder.
    */
  def apply(
             fsm: FSMInterface,
             wtd: Map[Int, WtDistribution],
             horizon: Int,
             confidenceThreshold: Double,
             maxSpread: Int,
             method: ForecastMethod
           ): WtForecasterBuilder = {
    val forecastsTable = buildForecastsTable(fsm, wtd, confidenceThreshold, maxSpread, method)
    new WtForecasterBuilder(wtd, forecastsTable, confidenceThreshold, maxSpread, method)
  }

  /**
    * This is where the forecast intervals are estimated and the predictions table constructed.
    *
    * @param fsm The FSM for which we want to estimate the forecast intervals.
    * @param wtDistributions The waiting-time distributions.
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The max spread.
    * @param method The forecast method, REGRESSION or CLASSIFICATION.
    * @return The predictions table.
    */
  private def buildForecastsTable(
                                   fsm: FSMInterface,
                                   wtDistributions: Map[Int, WtDistribution],
                                   confidenceThreshold: Double,
                                   maxSpread: Int,
                                   method: ForecastMethod
                                 ): Map[Int, Forecast] = {
    logger.info("Estimating forecasts...")
    val shortestPaths = if (fsm.shortestPathDistances.isEmpty) fsm.findShortestPathDistances else fsm.shortestPathDistances
    val forecasts = wtDistributions.map {
      case (x, y) => x -> {
        val center = if (shortestPaths.nonEmpty) shortestPaths(x) else -1
        val pred = y.buildForecast(center, method, confidenceThreshold, maxSpread)
        pred
      }
    }
    logger.info("done.")
    forecasts
  }
}

/**
  * Builder for waiting-time predictors.
  *
  * @param wtDistributions The waiting-time distributions.
  * @param forecastsTable The table with the forecast intervals.
  * @param confidenceThreshold The forecast threshold.
  * @param maxSpread The max spread.
  * @param method The forecast method, REGRESSION or CLASSIFICATION.
  */
class WtForecasterBuilder private(
                                   wtDistributions: Map[Int, WtDistribution],
                                   forecastsTable: Map[Int, Forecast],
                                   confidenceThreshold: Double,
                                   maxSpread: Int,
                                   method: ForecastMethod
                                 ) {

  // Basically sets the forecast method.
  // This does not actually affect the forecast intervals. These must have already been estimated.
  // However, we need the predictor to carry the information about the forecast method because we need to know how to
  // interpret and evaluate those intervals.
  // See model.predictor.runtime.PredictorRun. Determines what type of profiler.PredictionsHolder to use based on the
  // forecast method.
  val wtForecaster: WtForecaster =
    if (ForecastMethod.isClassification(method)) WtForecaster(forecastsTable, ForecasterType.CLASSIFICATION)
    else WtForecaster(forecastsTable, ForecasterType.REGRESSION)

  /**
    * Writes the distributions and the forecast intervals to a file.
    *
    * @param fn the path to the file.
    */
  def write2File(fn: String): Unit = {
    val sorted1 = SortedMap(wtDistributions.toSeq: _*)
    val writer = new FileWriter(fn, true)
    var row = ""
    for ((statek, statev) <- sorted1) {
      val wts = statev.getDistributionSorted
      val len = wts.size
      row = statek.toString + "," + confidenceThreshold.toString + "," + maxSpread.toString + "," + len.toString
      val pred = forecastsTable(statek)
      row += "," + pred.start.toString + "," + pred.end.toString
      for ((time, prob) <- wts) {
        row += "," + prob.toString
      }
      writer.write(row + "\n")
    }
    writer.close()
  }

}
