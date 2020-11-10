package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * A predicate that checks the type of the event and evaluates to true if it is equal to the given event type.
  * @param arguments 0 is the given event type
  */
case class IsEventTypePredicate(arguments: List[String]) extends Predicate {
  require(arguments.size == 1)
  val givenType: String = arguments(0)

  override def evaluate(event: GenericEvent): Boolean = event.eventType == givenType

  override def toString: String = "IsEventTypePredicate(" + list2Str(arguments, ",") + ")"
}
