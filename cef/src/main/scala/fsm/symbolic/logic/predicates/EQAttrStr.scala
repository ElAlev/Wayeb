package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

case class EQAttrStr(override val arguments: List[String]) extends Predicate(arguments) {
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    require(arguments.size == 2)
    if (valuation.hasRegister(arguments(1))) {
      val attributeValue: String = event.getValueOf(arguments(0)).toString
      val storedEvent = valuation.v(arguments(1))
      val registerValue: String = storedEvent.getValueOf(arguments(0)).toString
      attributeValue == registerValue
    }
    else false
  }

  override def toString: String = "EQAttrStr(" + list2Str(arguments, ",") + ")"
}

