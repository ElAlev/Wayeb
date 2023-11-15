package workflow.provider

import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource
import workflow.condition.Condition
import workflow.provider.source.order.{OrderSource, OrderSourceCrossVal, OrderSourceDirect}
import workflow.task.estimatorTask.OrderCrossValTask

object OrderProvider {
  /**
    * Constructor for oder provider.
    *
    * @param orderSource The source for the order:
    *                    - OrderSourceDirect for a direct simple number.
    *                    - OrderSourceCrossVal when order estimation must be done by cross validation.
    * @return An order provider.
    */
  def apply(orderSource: OrderSource): OrderProvider = new OrderProvider(orderSource, List.empty[Condition])
}

/**
  * Provider for the best order of a FSM.
  *
  * @param orderSource The source for the order:
  *                    - OrderSourceDirect for a direct simple number.
  *                    - OrderSourceCrossVal when order estimation must be done by cross validation.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class OrderProvider private (
                              orderSource: OrderSource,
                              conditions: List[Condition]
                            ) extends AbstractProvider(conditions) {

  /**
    * According to type of source for the order, builds a provider.
    *
    * @return The best order.
    */
  override def provide(): Integer = {
    orderSource match {
      case x: OrderSourceDirect => x.order
      case x: OrderSourceCrossVal => estimateOrder(x.fsmType, x.patternFile, x.declarations, x.streamSource, x.policy)
      case _ => throw new Error("Not valid OrderSource")
    }
  }

  /**
    * Estimates the best order for a pattern.
    *
    * @param fsmType The type of pattern, classical or symbolic.
    * @param patternFile The path to the pattern file.
    * @param declarations The path to the declarations file.
    * @param streamSource The source for the stream.
    * @param policy The counting policy.
    * @return The optimal order.
    */
  private def estimateOrder(
                             fsmType: String,
                             patternFile: String,
                             declarations: String,
                             streamSource: StreamSource,
                             policy: CountPolicy
                           ): Int = {
    val cvt = OrderCrossValTask(fsmType, patternFile, declarations, streamSource, policy)
    cvt.execute()
  }
}
