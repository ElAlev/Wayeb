package model.forecaster.next

import fsm.FSMInterface
import model.markov.MarkovChain
import model.forecaster.runtime.Forecast

object NextForecasterBuilder {

  /**
    * Creates a constructor for a next forecaster for a given FSM.
    *
    * @param fsm The FSM.
    * @param mc The FSM's Markov chain.
    * @return The forecaster builder.
    */
  def apply(
             fsm: FSMInterface,
             mc: MarkovChain
           ): NextForecasterBuilder = {
    val finalStates = fsm.getFinals
    val allStates = fsm.getStates
    val probs = allStates.map(nfs => (nfs, mc.getTransProbToStates(nfs, finalStates))).toMap
    val predictionsTable = probs.mapValues(prob => Forecast(1, 1, 1, prob))
    new NextForecasterBuilder(predictionsTable)
  }

}

/**
  * Builder for next forecasters.
  *
  * @param forecastsTable The table of single-point forecasts for the next predictor to be built.
  */
class NextForecasterBuilder private(forecastsTable: Map[Int, Forecast]) {

  /**
    * Actually creates the forecaster.
    *
    * @return The next forecaster.
    */
  def createForecaster(): NextForecaster = {
    val np = NextForecaster(forecastsTable)
    np
  }

}
