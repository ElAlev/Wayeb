package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

case class EQStr(arguments: List[String]) extends Predicate {
  override def evaluate(event: GenericEvent): Boolean = {
    require(arguments.size == 2)
    val variableValue = event.getValueOf(arguments.head).toString
    val constant = arguments(1)
    variableValue == constant
  }

  override def toString: String = "EQStr(" + list2Str(arguments, ",") + ")"

}
