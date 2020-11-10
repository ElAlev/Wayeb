package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Checks if the value of a certain event attribute (must be Double) falls within a range [min,max).
  * @param arguments 0 is the attribute name to be checked
  *                  1 is the min of the range
  *                  2 the max
  */
case class BT(arguments: List[String]) extends Predicate {

  override def evaluate(event: GenericEvent): Boolean = {
    require(arguments.size == 3)
    val attr = arguments(0)
    val min = arguments(1).toDouble
    val max = arguments(2).toDouble
    val variableValue = event.getValueOf(attr).toString.toDouble
    variableValue >= min & variableValue < max
  }

  override def toString: String = "BT(" + list2Str(arguments, ",") + ")"

}
