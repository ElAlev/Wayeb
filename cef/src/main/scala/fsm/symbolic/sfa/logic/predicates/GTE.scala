package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Checks if the value of a certain event attribute (must be Double) is above or equal a given constant.
  * @param arguments 0 is the attribute name to be checked
  *                  1 the given constant
  */
case class GTE(arguments: List[String]) extends Predicate {
  override def evaluate(event: GenericEvent): Boolean = {
    require(arguments.size == 2)
    val variableValue = event.getValueOf(arguments(0)).toString.toDouble
    val constant = arguments(1).toDouble
    variableValue >= constant
  }

  override def toString: String = "GTE(" + list2Str(arguments, ",") + ")"

}
