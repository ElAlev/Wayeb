package fsm.runtime

import ui.ConfigUtils

import scala.collection.mutable.Map

/**
  * A pool of runs for a FSM.
  *
  * @param fsmId The id of the FSM.
  * @param runReg The run registry from which we can retrieve the FSM's prototype. Note that this registry must contain
  *               a prototype for this FSM.
  */
class RunSubPool private[fsm] (
                                fsmId: Int,
                                runReg: RunRegistry
                              ) {
  // Active runs are locked, i.e., not available for reuse. Key is the timestamp when the run was created or assigned to
  // a new partition value.
  private val locked = Map[Long, Run]()
  // Inactive and expired runs are unlocked, i.e., available for reuse. Key is the time the run was collected.
  private val unlocked = Map[Long, Run]()
  // For each value of the partition attribute, we have a run. We also need this map which is keyed on the partition
  // attribute for efficiency reasons. See fsm.runtime.RunSubPool.existsRunWithAttributeVal and
  // fsm.runtime.RunSubPool.getRunByAttribute.
  private val lockedByAttribute = Map[String, Run]()
  private var expirationDeadline: Long = ConfigUtils.defaultExpiration

  /**
    * Checks whether there already exists an active run for the given value of the partition attribute.
    * @param attributeVal The given value of the partition attribute.
    * @return True if such a run already exists.
    */
  def existsRunWithAttributeVal(attributeVal: String): Boolean = lockedByAttribute.contains(attributeVal)

  /**
    * Retrieves an already existing active run for the given value of the partition attribute. Assumes that such a run
    * already exists.
    * @param attributeVal The given value of the partition attribute.
    * @return The run corresponding to the value of the partition attribute.
    */
  def getRunByAttribute(attributeVal: String): Run = {
    require(lockedByAttribute.contains(attributeVal))
    lockedByAttribute(attributeVal)
  }

  /**
    * Returns a run for the given value of the partition attribute. Either a new run is created or an "expired" one is
    * "recycled" and returned.
    *
    * @param timestamp The timestamp of the new event.
    * @param attributeVal The value of the partition attribute of the new event.
    * @return A run for the partition value.
    */
  def checkOut(
                timestamp: Long,
                attributeVal: String
              ): Run = {
    var returnRun: Run = null
    if (unlocked.nonEmpty) {
      // if there are expired runs, use one of them
      val (ts, oldRun) = unlocked.head
      unlocked.remove(ts)
      // but we need to set its partition value
      oldRun.setAttributeValue(attributeVal)
      // now we need to move the run to the set of locked runs
      locked += (timestamp -> oldRun)
      lockedByAttribute += (attributeVal -> oldRun)
      returnRun = oldRun
    } else {
      // otherwise, we need to create a new run through cloning
      val newRun = runReg.findAndClone(fsmId)
      newRun.setAttributeValue(attributeVal)
      locked += (timestamp -> newRun)
      lockedByAttribute += (attributeVal -> newRun)
      returnRun = newRun
    }
    returnRun
  }

  /**
    * Same as fsm.runtime.RunSubPool#checkOut(long, java.lang.String, scala.collection.immutable.Set),
    * but also allows for registering listeners to the run.
    *
    * @param timestamp The timestamp of the new event.
    * @param attributeVal The value of the partition attribute of the new event.
    * @param listeners The listeners to be registered.
    * @return A run for the partition value with the listeners registered.
    */
  def checkOut(
                timestamp: Long,
                attributeVal: String,
                listeners: Set[RunListener]
              ): Run = {
    val returnRun = checkOut(timestamp, attributeVal)
    for (rl <- listeners) returnRun.register(rl)
    returnRun
  }

  /**
    * Checks which runs have expired according to the given timestamp and makes them available for reuse.
    *
    * @param currentTimestamp The given timestamp.
    */
  def unlockExpired(currentTimestamp: Long): Unit = {
    if (expirationDeadline == -1) return
    var counter = 0
    for ((creationTimestamp, r) <- locked) {
      val timeElapsed = currentTimestamp - creationTimestamp
      if (timeElapsed > expirationDeadline) {
        // We recycle the run if the time elapsed since its creation or assignment is greater than the deadline.
        //TODO: Maybe do not use timestamp as unique id. We might have multiple events at the same time.
        counter += 1
        locked.remove(creationTimestamp)
        lockedByAttribute.remove(r.getAttributeValue)
        r.reset()
        unlocked += (currentTimestamp + counter -> r)
      }
    }
  }

  /**
    * Sets the expiration deadline.
    *
    * @param ed The new expiration deadline.
    */
  def setExpirationDeadline(ed: Long): Unit = expirationDeadline = ed

  /**
    * Shuts down the subpool.
    */
  def shutdown(): Unit = {
    locked.foreach(r => r._2.shutdown())
    unlocked.foreach(r => r._2.shutdown())
  }

  /**
    * @return The number of locked/unlocked runs.
    */
  def getSize: (Int, Int) = (locked.size, unlocked.size)
}
