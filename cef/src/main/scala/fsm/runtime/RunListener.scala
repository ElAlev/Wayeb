package fsm.runtime

/**
  * Every class that must monitor a run should implement this trait.
  */
trait RunListener {
  /**
    * Method to determine how a new event should be processed.
    * @param rm The message received from the run.
    */
  def newEventProcessed(rm: RunMessage): Unit

  /**
    * Method to determine what happens when we need to shutdown.
    */
  def shutdown(): Unit
}
