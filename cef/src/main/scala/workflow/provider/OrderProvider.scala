package workflow.provider

import workflow.condition.Condition
import workflow.provider.source.order.{OrderSource, OrderSourceDirect}

object OrderProvider {
  /**
    * Constructor for oder provider.
    *
    * @param orderSource The source for the order:
    *                    - OrderSourceDirect for a direct simple number.
    * @return An order provider.
    */
  def apply(orderSource: OrderSource): OrderProvider = new OrderProvider(orderSource, List.empty[Condition])
}

/**
  * Provider for the best order of a FSM.
  *
  * @param orderSource The source for the order:
  *                    - OrderSourceDirect for a direct simple number.
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
      case _ => throw new Error("Not valid OrderSource")
    }
  }

}
