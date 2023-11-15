package fsm.runtime

import fsm.FSMInterface
import fsm.symbolic.sfa.IdGenerator
import fsm.symbolic.sra.Configuration
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
  * Class to manage runs. Runs are stored and managed in a pool. The pool allows us to recycle certain "expired" or
  * killed runs. We can set an expiration deadline. If a run has been alive for more time than that of the deadline,
  * then the run is "killed" and returned to a set of unlocked runs. A run may also be killed if it corresponds to a
  * windowed FSM.
  * Each FSM has its own subpool of runs.
  *
  * @param fsms The list of FSMs for which we are going to be creating runs.
  * @param expirationDeadline The expiration deadline.
  * @param distance The distances allowed to emit forecasts.
  * @param show Determines whether complex event matches and forecasts are to be displayed or not by a run.
  */
class RunPool(
               fsms: List[FSMInterface],
               expirationDeadline: Long,
               distance: (Double, Double),
               show: Boolean
             ) extends Shutdownable {
  private val runReg: RunRegistry = new RunRegistry()
  private val runIdGenerator = new IdGenerator(ConfigUtils.idGeneratorMax, Set.empty[Int])//IdGenerator(1000000)
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
      val r = Run(runIdGenerator.getIdCautiousImmut, fsm, distance, show)
      runReg.addPrototype(r)
    }
    // then, for each FSM create a subpool
    for (fsm <- fsms) {
      val fsmId = fsm.getId
      val rsp = new RunSubPool(fsmId, runReg, runIdGenerator)
      rsp.setExpirationDeadline(ed)
      tmpPool += (fsmId -> rsp)
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
  def getRunsByAttribute(
                          fsmId: Int,
                          attributeVal: String
                        ): List[Run] = {
    require(pool.contains(fsmId))
    pool(fsmId).getRunsByAttribute(attributeVal)
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
   * Based on a given run, creates a clone and starts the new run with a given configuration.
   *
   * @param run          The given run to be cloned.
   * @param timestamp    The timestamp of the new event.
   * @param attributeVal The value of the partition attribute of the new event.
   * @param conf         The initial configuration for the new run.
   * @return The new run.
   */
  def checkOut(
                run: Run,
                attributeVal: String,
                timestamp: Long,
                conf: Configuration
              ): Run = {
    val fsmId = run.getFsmId
    require(pool.contains(fsmId))
    val rsp = pool(fsmId)
    rsp.checkOut(run, timestamp, attributeVal, conf)
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

  def reset(fsmId: Int): Unit = {
    val rsp = pool(fsmId)
    rsp.reset()
  }

  /**
   * Removes a given run from the pool of available runs.
   *
   * @param fsmId The id of the fsm corresponding to the run that is to be removed.
   * @param attributeVal The value of the partition attribute of the run.
   * @param runId        The id of the run to be removed.
   */
  def release(
               fsmId: Int,
               attributeVal: String,
               runId: Int
             ): Unit = {
    val rsp = pool(fsmId)
    rsp.release(attributeVal, runId)
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
