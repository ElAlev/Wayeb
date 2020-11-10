package model.forecaster

import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.wt.WtForecaster
import model.forecaster.runtime.RelativeForecast

object WtInterface {

  /**
    * Constructor for waiting-time interface.
    *
    * @param id The id of the FSM for which this predictor was constructed.
    * @param wtp The waiting-time predictor.
    * @param maxSpread The max spread of the forecast intervals.
    * @return The waiting-time interface.
    */
  def apply(
             id: Int,
             wtp: WtForecaster,
             maxSpread: Int
           ): WtInterface = new WtInterface(id, wtp, maxSpread)

}

/**
  * Interface for waiting-time predictors.
  *
  * @param id The id of the FSM for which this predictor was constructed.
  * @param wtp The waiting-time predictor.
  * @param maxSpread The max spread of the forecast intervals.
  */
class WtInterface private (
                            val id: Int,
                            val wtp: WtForecaster,
                            maxSpread: Int
                          ) extends ForecasterInterface with Serializable {

  /**
    * Retrieves a forecast interval from the predictions table for a given state.
    *
    * @param state The id of the given state.
    * @param eventCounter The event counter (index) of the event that triggered the forecast.
    * @return The forecast.
    */
  override def getNewForecast(
                                 state: Int,
                                 eventCounter: Long
                               ): RelativeForecast = {
    val tablePred = wtp.getForecastFromTable(state)
    RelativeForecast(startRelativeToNow     = tablePred.start,
                       endRelativeToNow       = tablePred.end,
                       middle                 = tablePred.middle,
                       prob                   = tablePred.prob,
                       startRelativeToCounter = eventCounter + tablePred.start,
                       endRelativeToCounter   = eventCounter + tablePred.end,
                       isPositive             = tablePred.positive
    )
  }

  override def getId: Int = id

  override def getStates: Set[Int] = wtp.getStates

  override def getMaxSpread: Int = maxSpread

  override def getType: ForecasterType = wtp.forecasterType

}
