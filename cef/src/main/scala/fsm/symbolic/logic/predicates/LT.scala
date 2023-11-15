package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Checks if the value of a certain event attribute (must be Double) is below a given constant.
  * @param arguments 0 is the attribute name to be checked
  *                  1 the given constant
  */
case class LT(override val arguments: List[String]) extends Predicate(arguments) {
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    require(arguments.size == 2)
    val variableValue = event.getValueOf(arguments(0)).toString.toDouble
    val constant = arguments(1).toDouble
    variableValue < constant
  }

  override def toString: String = "LT(" + list2Str(arguments, ",") + ")"

}
