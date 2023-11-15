package model.forecaster.runtime

import com.typesafe.scalalogging.LazyLogging
import fsm.runtime.{RunListener, RunMessage}
import model.forecaster.{HMMInterface, NextInterface, ForecasterInterface, ForecasterType}
import profiler.ForecastCollector
import profiler.classification.ClassificationForecastCollector
import profiler.regression.RegressionForecastCollector

object ForecasterRun {
  /**
    * Constructor for predictor run.
    *
    * @param pi The predictor interface that will be producing forecasts.
    * @param collectStats If true, the predictor run will be collecting evaluation statistics. Useful for testing but
    *                     affects performance.
    * @param finalsEnabled If true, final states are also allowed to emit forecasts.
    * @return A predictor run.
    */
  def apply(
             id: Int,
             pi: ForecasterInterface,
             collectStats: Boolean,
             finalsEnabled: Boolean
           ): ForecasterRun = new ForecasterRun(id, pi, collectStats, finalsEnabled)
}

/**
  * Class for representing predictor runs. Each FSM run should have attached to it a predictor run (when forecasting is
  * enabled). Predictor runs are listeners of FSM runs. Whenever the FSM run sends a RunMessage to its listeners, the
  * predictor run executes model.predictor.runtime.PredictorRun#newEventProcessed(fsm.runtime.RunMessage).
  *
  * @param pi The predictor interface that will be producing forecasts.
  * @param collectStats If true, the predictor run will be collecting evaluation statistics. Useful for testing but
  *                     affects performance.
  * @param finalsEnabled If true, final states are also allowed to emit forecasts.
  */
class ForecasterRun(
                     id: Int,
                     pi: ForecasterInterface,
                     collectStats: Boolean,
                     finalsEnabled: Boolean
                   ) extends RunListener with LazyLogging with ForecasterPrototype {

  // This collector collects run messages and predictions in order to evaluate them.
  private val collector: ForecastCollector = pi.getType match {
    case ForecasterType.REGRESSION => RegressionForecastCollector(pi.getId, pi.getStates)
    case ForecasterType.CLASSIFICATION => ClassificationForecastCollector(pi.getId)
  }
  private var prediction = RelativeForecast()

  /**
    * Executed whenever a FSM run sends a run message.
    *
    * @param rm The message received from the run.
    */
  def newEventProcessed(rm: RunMessage): Unit = {
    if (rm.isReset) collector.reset()
    else {
      pi match {
        case x: NextInterface => prediction = x.getNewForecast(rm.currentState, rm.eventCounter)
        case x: HMMInterface => {
          x.addObservation(rm.currentState)
          prediction = tryPredict(rm)
        }
        case _ => prediction = tryPredict(rm)
      }
    }
  }

  /**
    * Tries to emit a forecast. If it fails, produces a dummy, invalid forecast.
    *
    * @param rm The message received from the run.
    * @return The forecast.
    */
  private def tryPredict(rm: RunMessage): RelativeForecast = {
    val pred =
      // First, we need to determine whether the forecasting distances allow us to emit a forecast.
      // See fsm.runtime.Run.isEmitting.
      if (!rm.isEmitting) RelativeForecast()
      else {
        // If the distances are ok, we check the finals.
        // If finals are allowed, then we emit a forecast.
        if (finalsEnabled) pi.getNewForecast(rm.currentState, rm.eventCounter)
        else {
          // else, we only emit a forecast if we are not in a final state
          if (rm.fmDetected) RelativeForecast()
          else pi.getNewForecast(rm.currentState, rm.eventCounter)
        }
      }
    if (pred.isValid & rm.show) {
      val msg =
        "PREDICTION@" + rm.timestamp +
          ":Attr->" + rm.attributeValue +
          "|" + pred.toString //+
        //"\nFORECAST:" + pred.isPositive + "," + rm.lastEvent.getValueOf("positive")
      logger.info(msg)
    }
    if (collectStats) collector.collect(rm, pred)
    pred
  }

  def shutdown(): Unit = {}

  override def cloneForecaster(newId: Int): ForecasterRun = ForecasterRun(newId, pi, collectStats, finalsEnabled)

  override def getInterfaceId: Int = pi.getId

  def getCollector: ForecastCollector = collector

  def getCurrentPrediction: RelativeForecast = prediction

}
