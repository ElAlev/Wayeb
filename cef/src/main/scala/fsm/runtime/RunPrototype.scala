package fsm.runtime

/**
  * Each run prototype must implement this trait.
  */
trait RunPrototype {
  /**
    * A run prototype must be able to clone itself.
    * @return A new run.
    */
  def cloneRun(): Run

  /**
    * @return The id of the run's FSM.
    */
  def getId: Int
}
