package fsm.symbolic

/**
 * Represents the "output" emitted by a transition.
 * Essentially marks a transition as being a TAKE one, meaning that the triggering event is part of the match, or as
 * being an IGNORE one, meaning that the event is irrelevant and must be skipped.
 */
object TransitionOutput extends Enumeration {
  type TransitionOutput = Value
  val TAKE, IGNORE = Value
}
