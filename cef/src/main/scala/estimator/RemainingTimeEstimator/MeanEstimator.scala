package estimator.RemainingTimeEstimator

import estimator.RunEstimator
import fsm.FSMInterface
import fsm.runtime.RunMessage
import scala.collection.mutable.ListBuffer
import breeze.stats.{mean, stddev}

object MeanEstimator {
  /**
    * Constructor for Mean estimator.
    *
    * @param fsm The FSM for which we want to build the model.
    * @return The estimator.
    */
  def apply(fsm: FSMInterface): MeanEstimator = new MeanEstimator(fsm)
}

/**
  * Builds a prediction model based on (not a direct implementation)
  * Wil M. P. van der Aalst, M. H. Schonenberg, and Minseok Song. Time prediction based on
  * process mining. Inf. Syst., 36(2):450–475, 2011
  * For each automaton state, we count the number of events until we reach a final state, called the "remaining
  * time". We then estimate the mean remaining time. Based on the mean remaining time and the standard deviation,
  * we can fit a gaussian to obtain the waiting-time distribution.
  *
  * @param fsm The FSM for which we want to build the model.
  */
class MeanEstimator private (fsm: FSMInterface) extends RunEstimator with Serializable {
  // for each state, maintain a buffer with the remaining times
  private var measurements: Map[Int, ListBuffer[Double]] = fsm.getStates.map(e => (e, ListBuffer.empty[Double])).toMap
  //private val initRts: Map[Int,(Double,Double)] = fsm.getStates.map(e => (e,(-1.0,-1.0))).toMap
  private val meanRts: RemainingTimes = new RemainingTimes(Map.empty)

  override def newEventProcessed(rm: RunMessage): Unit = {
    if (!rm.isReset & rm.fmDetected) {
      val storedEvents = rm.matchedEvents.getEventsWithInfo
      for (e <- storedEvents) {
        val state = e._3.stateId
        val counters = measurements(state)
        val elapsedTime = (rm.eventCounter - e._2).toDouble
        counters += elapsedTime
        measurements += (state -> counters)
      }
    }
  }

  override def estimate(): Unit = {
    for (measurement <- measurements) {
      val state = measurement._1
      val meanRemainingTime = mean(measurements(state))
      val stddevRemainingTime = stddev(measurements(state))
      if (meanRemainingTime != 0) meanRts.updateState(state, meanRemainingTime, stddevRemainingTime)
    }
  }

  override def shutdown(): Unit = {}

  def getRemainingTimes: RemainingTimes = meanRts

}
