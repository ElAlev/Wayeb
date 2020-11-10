package fsm.runtime

/**
  * The creation of FSM runs follows the Prototype creational design pattern.
  * See https://sourcemaking.com/design_patterns/prototype.
  * For each pattern/FSM, we maintain a prototype.
  * Whenever we need to create a new run, we clone the prototype.
  */
class RunRegistry {
  // The prototypes. Each pattern/FSM has a unique id.
  private var prototypes = Map[Int, RunPrototype]()

  /**
    * Adds a new prototype.
    *
    * @param rp The new prototype.
    */
  def addPrototype(rp: RunPrototype): Unit = {
    val k = rp.getId
    if (prototypes.contains(k))
      throw new IllegalArgumentException("There already exists a RunPrototype with id: " + k)
    prototypes += (k -> rp)
  }

  /**
    * Creates a run for a FSM by cloning the prototype.
    *
    * @param id The id of the FSM.
    * @return The new run.
    */
  def findAndClone(id: Int): Run = {
    if (!prototypes.contains(id))
      throw new IllegalArgumentException("RunRegistry has no prototypes for: " + id)
    val rp = prototypes(id)
    rp.cloneRun()
  }

  /**
    * Clear all prototypes.
    */
  def clear(): Unit = prototypes = Map[Int, RunPrototype]()

  override def toString: String = prototypes.toString()

}
