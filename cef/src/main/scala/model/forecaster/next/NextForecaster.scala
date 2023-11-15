package model.forecaster.next

import model.forecaster.ForecasterType
import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.runtime.Forecast

object NextForecaster {
  /**
    * Constructs a next forecaster.
    *
    * @param predictionsTable The table of single-point predictions.
    * @return The next forecaster.
    */
  private[next] def apply(predictionsTable: Map[Int, Forecast]): NextForecaster =
    new NextForecaster(predictionsTable, ForecasterType.REGRESSION)
}

/**
  * This is a forecaster that predicts how probable it is to reach a final state at the next point.
  * Its forecasts are always single point, referring to the next point in the stream.
  *
  * @param forecastsTable The table of single-point forecasts.
  * @param forecasterType Should always be REGRESSION.
  */
class NextForecaster private(
                              val forecastsTable: Map[Int, Forecast],
                              val forecasterType: ForecasterType
                            ) {

  /**
    * Retrieves the forecast interval (which is single-point actually) for a given state from the table of predictions.
    *
    * @param state The id of the given state.
    * @return The forecast interval.
    */
  def getForecastFromTable(state: Int): Forecast = {
    val tablePred = forecastsTable(state)
    tablePred
  }

  /**
    * @return The ids of all the states for which we have entries in the predictions table.
    */
  def getStates: Set[Int] = forecastsTable.keySet
}
