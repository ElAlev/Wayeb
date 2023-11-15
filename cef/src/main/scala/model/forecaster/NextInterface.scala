package model.forecaster

import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.next.NextForecaster
import model.forecaster.runtime.RelativeForecast

object NextInterface {

  /**
    * Constructor for next point predictor interface.
    *
    * @param id The id of the FSM for which this predictor was constructed.
    * @param np The next predictor.
    * @return The next point interface.
    */
  def apply(
             id: Int,
             np: NextForecaster
           ): NextInterface = new NextInterface(id, np)
}

/**
  * Interface for next point predictors.
  *
  * @param id The id of the FSM for which this predictor was constructed.
  * @param np The next predictor.
  */
class NextInterface private (
                              val id: Int,
                              val np: NextForecaster
                            ) extends ForecasterInterface {

  /**
    * Retrieves a forecast interval from the predictions table for a given state.
    *
    * @param state The given state.
    * @param eventCounter The event counter (index) of the event that triggered the forecast.
    * @return The forecast.
    */
  override def getNewForecast(
                                 state: Int,
                                 eventCounter: Long
                               ): RelativeForecast = {
    val tablePred = np.getForecastFromTable(state)
    RelativeForecast(startRelativeToNow     = tablePred.start,
                       endRelativeToNow       = tablePred.end,
                       middle                 = tablePred.middle,
                       prob                   = tablePred.prob,
                       startRelativeToCounter = eventCounter + tablePred.start,
                       endRelativeToCounter   = eventCounter + tablePred.end
    )
  }

  override def getId: Int = id

  override def getStates: Set[Int] = np.getStates

  override def getMaxSpread: Int = -1

  override def getType: ForecasterType = np.forecasterType

}
