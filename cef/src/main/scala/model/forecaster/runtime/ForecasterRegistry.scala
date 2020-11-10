package model.forecaster.runtime

import com.typesafe.scalalogging.LazyLogging

/**
  * The creation of forecaster runs follows the Prototype creational design pattern.
  * See https://sourcemaking.com/design_patterns/prototype.
  * For each pattern/FSM, we maintain a prototype.
  * Whenever we need to create a new forecaster run, we clone the prototype.
  */
class ForecasterRegistry() extends LazyLogging {

  // For each FSM, we have a prototype. The key is the FSM id.
  private var prototypes = Map[Int, ForecasterPrototype]()

  /**
    * Adds a new prototype.
    *
    * @param pp The new prototype.
    */
  def addPrototype(pp: ForecasterPrototype): Unit = {
    val k = pp.getId
    if (prototypes.contains(k)) {
      logger.error("There already exists a ForecasterPrototype with name: " + k)
      throw new IllegalArgumentException("There already exists a ForecasterPrototype with name: " + k)
    }
    prototypes += (k -> pp)
  }

  /**
    * Creates a forecaster run for a FSM by cloning the prototype.
    *
    * @param id The id of the FSM.
    * @return The new predictor run.
    */
  def findAndClone(id: Int): ForecasterRun = {
    if (!prototypes.contains(id)) {
      logger.error("ForecasterRegistry has no prototypes for: " + id)
      throw new IllegalArgumentException("ForecasterRegistry has no prototypes for: " + id)
    }
    val pp = prototypes(id)
    pp.cloneForecaster
  }

}
