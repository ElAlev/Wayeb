package stream.source

import stream.GenericEvent

trait StreamListener {
  def newEventEmitted(event: GenericEvent): Unit
}
