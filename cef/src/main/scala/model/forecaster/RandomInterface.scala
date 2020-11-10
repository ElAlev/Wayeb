package model.forecaster

import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.random.RandomForecaster
import model.forecaster.runtime.RelativeForecast

object RandomInterface {

  /**
    * Constructor for random predictor interface.
    *
    * @param id The id of the FSM for which this predictor was constructed.
    * @param rp The random predictor.
    * @param states The states from which we want to emit forecasts. Make sure they are actually FSM states.
    * @return The random predictor interface.
    */
  def apply(
             id: Int,
             rp: RandomForecaster,
             states: Set[Int]
           ): RandomInterface = new RandomInterface(id, rp, states)

}

/**
  * Interface for random predictors.
  *
  * @param id The id of the FSM for which this predictor was constructed.
  * @param rp The random predictor.
  * @param states The states from which we want to emit forecasts. Make sure they are actually FSM states.
  */
class RandomInterface private (
                                val id: Int,
                                val rp: RandomForecaster,
                                val states: Set[Int]
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
    val randPred = rp.getRandForecast
    RelativeForecast(startRelativeToNow     = randPred.start,
                       endRelativeToNow       = randPred.end,
                       middle                 = randPred.middle,
                       prob                   = randPred.prob,
                       startRelativeToCounter = eventCounter + randPred.start,
                       endRelativeToCounter   = eventCounter + randPred.end
    )
  }

  override def getId: Int = id

  override def getStates: Set[Int] = states

  override def getMaxSpread: Int = rp.horizon

  override def getType: ForecasterType = rp.forecasterType
}
