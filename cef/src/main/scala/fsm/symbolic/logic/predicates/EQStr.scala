package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

case class EQStr(override val arguments: List[String]) extends Predicate(arguments) {
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    require(arguments.size == 2)
    val variableValue = event.getValueOf(arguments.head).toString
    val constant = arguments(1)
    variableValue == constant
  }

  override def toString: String = "EQStr(" + list2Str(arguments, ",") + ")"

}
