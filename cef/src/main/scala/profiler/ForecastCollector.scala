package profiler

import fsm.runtime.RunMessage
import model.forecaster.runtime.RelativeForecast

/**
  * Collectors must inherit from this class.
  *
  * The main job of a forecast collector is to collect the produced forecasts and detections of its run. Each predictor
  * run creates internally a collector.
  */
abstract class ForecastCollector {
  /**
    * The predictor run calls this method after every attempt to produce a forecast.
    *
    * @param rm The message sent from the FSM run to the predictor run.
    * @param forecast The forecast produced. Could be an empty one.
    */
  def collect(
               rm: RunMessage,
               forecast: RelativeForecast
             ): Unit

  /**
    * What happens when a RESET event arrives.
    */
  def reset(): Unit

}
