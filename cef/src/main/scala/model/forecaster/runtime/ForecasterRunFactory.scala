package model.forecaster.runtime

import model.forecaster.ForecasterInterface

object ForecasterRunFactory {
  /**
    * Constructor for predictor runs factory.
    *
    * @param pis The list of predictor interfaces which will act as prototypes for creating new predictor runs.
    * @param collectStats If true, the predictor runs will be collecting evaluation statistics. Useful for testing but
    *                     affects performance.
    * @param finalsEnabled If true, final states are also allowed to emit forecasts.
    * @return The factory.
    */
  def apply(
             pis: List[ForecasterInterface],
             collectStats: Boolean,
             finalsEnabled: Boolean
           ): ForecasterRunFactory = new ForecasterRunFactory(pis, collectStats, finalsEnabled)
}

/**
  * Factory for creating predictor runs.
  *
  * @param pis The list of predictor interfaces which will act as prototypes for creating new predictor runs.
  * @param collectStats If true, the predictor runs will be collecting evaluation statistics. Useful for testing but
  *                     affects performance.
  * @param finalsEnabled If true, final states are also allowed to emit forecasts.
  */
class ForecasterRunFactory(
                            pis: List[ForecasterInterface],
                            collectStats: Boolean,
                            finalsEnabled: Boolean
                          ) {
  private val foreReg = initialize(pis)

  /**
    * Initializes the factory by creating registry of prototypes/
    * @param pis The list of predictor interfaces which will act as prototypes for creating new predictor runs.
    * @return The prototypes registry.
    */
  private def initialize(pis: List[ForecasterInterface]): ForecasterRegistry = {
    val tmpReg = new ForecasterRegistry()
    pis.foreach(pi => tmpReg.addPrototype(new ForecasterRun(pi, collectStats, finalsEnabled)))
    tmpReg
  }

  /**
    * Creates a new predictor run for a given FSM.
    *
    * @param id The id of the given FSM.
    * @return A new predictor run for the given FSM.
    */
  def getNewForecasterRun(id: Int): ForecasterRun = foreReg.findAndClone(id)
}
