package estimator

import fsm.FSMInterface
import fsm.runtime.RunPool
import stream.GenericEvent
import stream.source.{EndOfStreamEvent, StreamListener}
import ui.ConfigUtils

object RunEstimatorEngine {
  /**
    * Constructor for estimator engine.
    *
    * @param fsm The FSM for which we want to estimate a learner.
    * @param learner The learner, which must be a RunListener, e.g., estimator.MatrixEstimator.MLEEstimator or
    *                estimator.RemainingTimeEstimator.MeanEstimator.
    * @return The engine.
    */
  def apply(
             fsm: FSMInterface,
             learner: RunEstimator
           ): RunEstimatorEngine = new RunEstimatorEngine(fsm, learner)
}

/**
  * The engine to run estimation tasks. Works similarly to engine.ERFEngine.
  *
  * @param fsm The FSM for which we want to estimate a learner.
  * @param learner The learner, which must be a RunListener, e.g., estimator.MatrixEstimator.MLEEstimator or
  *                estimator.RemainingTimeEstimator.MeanEstimator.
  */
class RunEstimatorEngine(
                          fsm: FSMInterface,
                          learner: RunEstimator
                        ) extends StreamListener {
  private val singlePartitionVal = ConfigUtils.singlePartitionVal
  private val rpt = RunPool(
    List(fsm),
    ConfigUtils.defaultExpiration,
    ConfigUtils.defaultDistance,
    show = false
  )

  override def newEventEmitted(event: GenericEvent): Unit = {
    event match {
      case x: EndOfStreamEvent => rpt.shutdown()
      case _ => processEvent(fsm, learner, event, rpt)
    }
  }

  def getLearner: RunEstimator = learner

  private def processEvent(
                            fsm: FSMInterface,
                            learner: RunEstimator,
                            e: GenericEvent,
                            rp: RunPool
                          ): Boolean = {
    var detected = false
    val partitionAttribute = fsm.getPartitionAttribute
    val av = if (partitionAttribute.equalsIgnoreCase(singlePartitionVal)) singlePartitionVal
    else e.getValueOf(partitionAttribute).toString
    if (rp.existsRunWithAttributeVal(fsm.getId, av)) {
      // CAUTION: Assumes deterministic automata used, thus only one run per partition attribute value.
      val r = rp.getRunsByAttribute(fsm.getId, av).head
      r.processEventDet(e)
      detected = r.ceDetected
    } else {
      val r1 = rp.checkOut(fsm.getId, av, e.timestamp)
      r1.register(learner)
      r1.processEventDet(e)
      detected = r1.ceDetected
    }
    detected
  }

}
