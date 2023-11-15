package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

case class GTAttr(override val arguments: List[String]) extends Predicate(arguments) {
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    require(arguments.size == 2)
    if (valuation.hasRegister(arguments(1))) {
      val attributeValue: Double = event.getValueOf(arguments(0)).toString.toDouble
      val storedEvent = valuation.v(arguments(1))
      val registerValue: Double = storedEvent.getValueOf(arguments(0)).toString.toDouble
      attributeValue > registerValue
    }
    else false
  }

  override def toString: String = "GTAttr(" + list2Str(arguments, ",") + ")"
}
