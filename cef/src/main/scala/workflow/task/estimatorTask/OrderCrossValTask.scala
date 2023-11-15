package workflow.task.estimatorTask

import com.typesafe.scalalogging.LazyLogging
import estimator.OrderEstimator.CrossValEstimator
import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource
import ui.ConfigUtils
import workflow.task.Task

object OrderCrossValTask {
  /**
    * Constructor for best order estimation task.
    *
    * @param fsmType The type of the FSM, classical or symbolic.
    * @param patternFile The path to the file with the FSM.
    * @param declarations The path to the declarations file.
    * @param streamSource The source for the stream.
    * @param policy The counting policy.
    * @return An order task.
    */
  def apply(
             fsmType: String,
             patternFile: String,
             declarations: String,
             streamSource: StreamSource,
             policy: CountPolicy
           ): OrderCrossValTask = new OrderCrossValTask(fsmType, patternFile, declarations, streamSource, policy)
}

/**
  * This task is responsible for estimating the optimal order among full-order Markov models for a FSM.
  *
  * @param fsmType The type of the FSM, classical or symbolic.
  * @param patternFile The path to the file with the FSM.
  * @param declarations The path to the declarations file.
  * @param streamSource The source for the stream.
  * @param policy The counting policy.
  */
class OrderCrossValTask private (
                                  fsmType: String,
                                  patternFile: String,
                                  declarations: String,
                                  streamSource: StreamSource,
                                  policy: CountPolicy
                                ) extends Task with LazyLogging {

  private val maxOrder = ConfigUtils.maxCrossValOrder

  override def execute(): Integer = {
    logger.info("Executing order estimation...")
    val cve = CrossValEstimator(fsmType, patternFile, declarations, policy, maxOrder, streamSource)
    // Works only for a single pattern.
    // If more than 1 pattern present in patternFile, only the first will be used.
    val bestOrder = cve.estimateOrder()
    logger.info("Best orders found: " + bestOrder)
    logger.info("done.")
    bestOrder
  }
}
