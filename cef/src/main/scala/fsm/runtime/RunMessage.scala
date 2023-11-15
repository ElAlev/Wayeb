package fsm.runtime

import fsm.symbolic.Valuation
import stream.GenericEvent

object RunMessage {
  def apply(
             fmDetected: Boolean,
             currentState: Int,
             previousState: Int,
             eventCounter: Long,
             timestamp: Long,
             matchedEvents: Match,
             attributeValue: String,
             isEmitting: Boolean,
             lastEvent: GenericEvent,
             valuation: Valuation,
             show: Boolean
           ): RunMessage = new RunMessage(
    fmDetected,
    currentState,
    previousState,
    eventCounter,
    timestamp,
    matchedEvents,
    attributeValue,
    isEmitting: Boolean,
    false,
    lastEvent,
    valuation,
    show
  )

  def ResetMessage(): RunMessage = new RunMessage(
    false,
    -1,
    -1,
    -1,
    -1,
    Match(),
    "",
    false,
    true,
    GenericEvent("", -1),
    Valuation(),
    false
  )
}

/**
  * Class representing the message that a run sends to its listeners.
  *
  * @param fmDetected Should be true if the run has detected a complex event / full match.
  * @param currentState The current state of the run.
  * @param previousState The previous state of the run.
  * @param eventCounter The event counter.
  * @param timestamp The timestamp of the new event that we processed.
  * @param matchedEvents The events of the (partial or full) match thus far.
  * @param attributeValue The attribute value.
  * @param isEmitting True if a forecast should be emitted.
  * @param isReset True if the event consumed was a reset event.
  * @param lastEvent The event just consumed.
  */
class RunMessage(
                  val fmDetected: Boolean,
                  val currentState: Int,
                  val previousState: Int,
                  val eventCounter: Long,
                  val timestamp: Long,
                  val matchedEvents: Match,
                  val attributeValue: String,
                  val isEmitting: Boolean,
                  val isReset: Boolean,
                  val lastEvent: GenericEvent,
                  val valuation: Valuation,
                  val show: Boolean
                ) {

}
