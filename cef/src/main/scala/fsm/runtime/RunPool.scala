package fsm.runtime

import fsm.FSMInterface
import ui.ConfigUtils
import utils.Shutdownable

object RunPool {
  def apply(fsms: List[FSMInterface]): RunPool =
    new RunPool(fsms, ConfigUtils.defaultExpiration, ConfigUtils.defaultDistance, ConfigUtils.defaultShowMatchesForecasts)

  def apply(
             fsms: List[FSMInterface],
             ed: Long,
             distance: (Double, Double),
             show: Boolean
           ): RunPool = new RunPool(fsms, ed, distance, show)
}

/**
  * Class to manage runs. Runs are stored and managed in a pool. The pool allows us to recycle certain "expired" runs.
  * We can set an expiration deadline. If a run has been alive for more time than that of the deadline, then the run is
  * "killed" and returned to a set of unlocked runs. Each FSM has its own subpool of runs.
  *
  * @param fsms The list of FSMs for which we are going to be creating runs.
  * @param expirationDeadline The expiration deadline.
  * @param distance The distances allowed to emit forecasts.
  */
class RunPool(
               fsms: List[FSMInterface],
               expirationDeadline: Long,
               distance: (Double, Double),
               show: Boolean
             ) extends Shutdownable {
  private val runReg: RunRegistry = new RunRegistry()
  private var pool: Map[Int, RunSubPool] = initialize(fsms, expirationDeadline, distance, show)
  //private val activeRuns = Map[String,Map[String,Run]]()

  private def initialize(
                          fsms: List[FSMInterface],
                          ed: Long,
                          distance: (Double, Double),
                          show: Boolean
                        ): Map[Int, RunSubPool] = {
    require(fsms.nonEmpty)
    require(ed >= -1 & ed != 0)
    var tmpPool = Map[Int, RunSubPool]()
    // first, created all prototypes
    for (fsm <- fsms) {
      val r = Run(fsm, distance, show)
      runReg.addPrototype(r)
    }
    // then, for each FSM create a subpool
    for (fsm <- fsms) {
      val id = fsm.getId
      val rsp = new RunSubPool(id, runReg)
      rsp.setExpirationDeadline(ed)
      tmpPool += (id -> rsp)
    }
    tmpPool
  }

  /**
    * Checks if there already exists a run for a given FSM and a given value of the partition attribute.
    *
    * @param fsmId The id of the FSM.
    * @param attributeVal The value of the partition attribute.
    * @return True if there does exist such a run.
    */
  def existsRunWithAttributeVal(
                                 fsmId: Int,
                                 attributeVal: String
                               ): Boolean = {
    require(pool.contains(fsmId))
    pool(fsmId).existsRunWithAttributeVal(attributeVal)
  }

  /**
    * Retrieves the run associated with a given FSM and a given value of the partition attribute.
    * There must exist such a run.
    *
    * @param fsmId The id of the FSM.
    * @param attributeVal The value of the partition attribute.
    * @return The (already existing) run corresponding to the given FSM and the given partition attribute value.
    */
  def getRunByAttribute(
                         fsmId: Int,
                         attributeVal: String
                       ): Run = {
    require(pool.contains(fsmId))
    pool(fsmId).getRunByAttribute(attributeVal)
  }

  /**
    * Creates a new run for a given FSM and a given value of the partition attribute.
    * Instead of creating a new Run object, a recycled one may be returned.
    *
    * @param fsmId The id of the FSM.
    * @param attributeVal The value of the partition attribute.
    * @param timestamp The timestamp of the event which triggered the request for a new run.
    * @return A new (created or recycled) run.
    */
  def checkOut(
                fsmId: Int,
                attributeVal: String,
                timestamp: Long
              ): Run = {
    require(pool.contains(fsmId))
    val rsp = pool(fsmId)
    rsp.checkOut(timestamp, attributeVal)
  }

  /**
    * Same as fsm.runtime.RunPool#checkOut(int, java.lang.String, long, scala.collection.immutable.Set),
    * but also registers listeners to the new run.
    *
    * @param fsmId The id of the FSM.
    * @param attributeVal The value of the partition attribute.
    * @param timestamp The timestamp of the event which triggered the request for a new run.
    * @param listeners The listeners to be registered.
    * @return A new (created or recycled) run with the listeners registered.
    */
  def checkOut(
                fsmId: Int,
                attributeVal: String,
                timestamp: Long,
                listeners: Set[RunListener]
              ): Run = {
    require(pool.contains(fsmId))
    val rsp = pool(fsmId)
    rsp.checkOut(timestamp, attributeVal, listeners)
  }

  /**
    * Collects expired runs.
    *
    * @param timestamp The timestamp against which we are going to check for expiration.
    */
  def runsCollect(timestamp: Long): Unit = pool.foreach(rsp => rsp._2.unlockExpired(timestamp))

  /**
    * @return locked/unlocked runs.
    */
  def getSize: Tuple2[Int, Int] = {
    var lockedSize = 0
    var unlockedSize = 0
    for ((pattern, rsp) <- pool) {
      val (thisLockedSize, thisUnlockedSize) = rsp.getSize
      lockedSize += thisLockedSize
      unlockedSize += thisUnlockedSize
    }
    (lockedSize, unlockedSize)
  }

  /**
    * Shuts down pool.
    */
  def shutdown(): Unit = {
    runReg.clear()
    pool.foreach(r => r._2.shutdown())
    pool = Map[Int, RunSubPool]()
  }

  override def toString: String = pool.toString() + "\n" + runReg.toString()

}
