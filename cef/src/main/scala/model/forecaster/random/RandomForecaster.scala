package model.forecaster.random

import model.forecaster.ForecasterType
import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.runtime.Forecast

object RandomForecaster {
  /**
    * Constructor for random predictor.
    *
    * @param seed The seed for the random number generators.
    * @param horizon The horizon.
    * @return The random predictor.
    */
  def apply(
             seed: Int,
             horizon: Int
           ): RandomForecaster = new RandomForecaster(seed, horizon, ForecasterType.REGRESSION)
}

/**
  * This is a predictor that produces random forecast intervals within the given horizon.
  *
  * @param seed The seed for the random number generators.
  * @param horizon The horizon.
  * @param forecasterType Should always be REGRESSION.
  */
class RandomForecaster private(
                                val seed: Int,
                                val horizon: Int,
                                val forecasterType: ForecasterType
                              ) {

  private val rs = new scala.util.Random(seed)
  private val re = new scala.util.Random(seed)

  /**
    * @return A random forecast interval within the horizon.
    */
  def getRandForecast: Forecast = {
    val start = rs.nextInt(horizon) + 1
    var end = start
    if (start != horizon) end = re.nextInt(horizon - start) + 1
    end += start
    Forecast(start, end, (start + end) / 2, 0.0)
  }

}
