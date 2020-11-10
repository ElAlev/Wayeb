package model.forecaster.wt

import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.runtime.Forecast

object WtForecaster {
  /**
    * Constructor for a waiting-time predictor.
    *
    * @param forecastsTable The table holding a forecast interval for each emitting state.
    * @param forecasterType REGRESSION or CLASSIFICATION.
    * @return The waiting-time predictor.
    */
  private[wt] def apply(
                         forecastsTable: Map[Int, Forecast],
                         forecasterType: ForecasterType
                       ): WtForecaster =
    new WtForecaster(forecastsTable, forecasterType)
}

/**
  * Predictor whose forecasts should be derived from waiting-time distributions.
  *
  * @param forecastsTable The table holding a forecast interval for each emitting state.
  * @param forecasterType REGRESSION or CLASSIFICATION.
  */
class WtForecaster private(
                            val forecastsTable: Map[Int, Forecast],
                            val forecasterType: ForecasterType
                          ) extends Serializable {

  /**
    * Retrieves the forecast interval from the table for a given state.
    *
    * @param state The id of the given state.
    * @return The forecast interval.
    */
  def getForecastFromTable(state: Int): Forecast = {
    val tablePred = forecastsTable(state)
    if (tablePred.start != -1) Forecast(
      start    = tablePred.start,
      end      = tablePred.end,
      middle   = tablePred.middle,
      prob     = tablePred.prob,
      positive = tablePred.positive
    )
    else Forecast()
  }

  /**
    * @return The ids of all the states for which we have entries in the predictions table.
    */
  def getStates: Set[Int] = forecastsTable.keySet
}
