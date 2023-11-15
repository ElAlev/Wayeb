package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Checks if the value of a certain event attribute (must be Double) falls within a range [min,max).
  * @param arguments 0 is the attribute name to be checked
  *                  1 is the min of the range
  *                  2 the max
  */
case class BT(override val arguments: List[String]) extends Predicate(arguments) {

  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    require(arguments.size == 3)
    val attr = arguments(0)
    val min = arguments(1).toDouble
    val max = arguments(2).toDouble
    val variableValue = event.getValueOf(attr).toString.toDouble
    variableValue >= min & variableValue < max
  }

  override def toString: String = "BT(" + list2Str(arguments, ",") + ")"

}
