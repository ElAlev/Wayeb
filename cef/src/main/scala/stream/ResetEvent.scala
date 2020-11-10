package stream

object ResetEvent {
  /**
    * Constructor for reset events.
    *
    * @param extraArgs The map of extra attributes, if any. Could be empty. The partition attribute may be here.
    * @return A reset event.
    */
  def apply(extraArgs: Map[String, Any]): ResetEvent = new ResetEvent(extraArgs)

  /**
    * Constructor for reset events.
    *
    * @return A reset event.
    */
  def apply(): ResetEvent = new ResetEvent(Map.empty)
}

/**
  * RESET events are special events. They are not part of the stream. They are inserted whenever we want to create a
  * stream from many different, smaller substream. Reset events are used to stitch together the substreams. They must
  * separate the substreams. A RESET event resets a run, i.e., discards partial matches and sends the run back to its
  * start state.
  *
  * @param extraArgs The map of extra attributes, if any. Could be empty. The partition attribute may be here.
  */
final class ResetEvent(extraArgs: Map[String, Any])
  extends GenericEvent(-1, "RESET", 0, extraArgs) {

}
