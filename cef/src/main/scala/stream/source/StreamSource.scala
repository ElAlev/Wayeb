package stream.source

import stream.source.EmitMode.EmitMode
import stream.GenericEvent
import stream.array.EventStream

/**
  * A stream to be consumed is first wrapped inside a stream source.
  * A source has to implement stream.source.StreamSource#emitEvents(scala.Enumeration.Value).
  * Typically, if mode is stream.source.EmitMode#BUFFER() then a stream in the form of an array is returned.
  * If mode is stream.source.EmitMode#ONLINE() emitEvents should call
  * stream.source.StreamSource#send2Listeners(stream.GenericEvent) for every event. In this case, events do not have to
  * be stored. A stream consumer that implements stream.source.StreamListener will be able to process every new event
  * as soon as it is emitted and does not have to store it. Useful for large (or unbounded) streams.
  */
abstract class StreamSource {

  private var listeners: Map[Int, StreamListener] = Map.empty

  /**
    * Every stream listener must first register itself with the stream source it wants to listen to.
    *
    * @param streamListener The listener to be registered.
    * @return The registration id assigned to this listener.
    */
  final def register(streamListener: StreamListener): Int = {
    val listenersMaxId = if (listeners.isEmpty) 0 else listeners.keySet.max
    val registrationId = listenersMaxId + 1
    listeners += (registrationId -> streamListener)
    registrationId
  }

  /**
    * After sending the stream to the listener, we can unregister it from the source.
    * CAUTION: After registering a listener to a stream source, do what you have to do with the listener and do NOT
    * forget to unregister it. Do not let the listener keep listening to the same source and use this source for other
    * listeners as well. When you replay the source, the events are sent to all of its listeners and this might not be
    * what you want to do.
    *
    * @param registrationId The listener's registration id.
    * @return The remaining listeners.
    */
  final def unregister(registrationId: Int): Map[Int, StreamListener] = {
    require(listeners.contains(registrationId))
    listeners -= registrationId
    listeners
  }

  /**
    * Sends a new event to all registered listeners.
    *
    * @param event The new event.
    */
  def send2Listeners(event: GenericEvent): Unit = listeners.values.foreach(_.newEventEmitted(event))

  /**
    * Sends a special end-of-stream event to all registered listeners. This event indicates that there are no more
    * events in the stream. Useful so as to know when to shutdown the forecasting engine.
    */
  private def sendEndOfStream(): Unit = send2Listeners(new EndOfStreamEvent)

  /**
    * Emits all events in the stream. If the mode is ONLINE, also sends an end-of-stream final event.
    *
    * @param mode The mode.
    * @return The event stream as an array (typically empty if mode is ONLINE).
    */
  final def emitEventsAndClose(mode: EmitMode): EventStream = {
    val es = emitEvents(mode)
    if (mode == EmitMode.ONLINE) sendEndOfStream()
    es
  }

  /**
    * Registers a listener and emits all events in ONLINE mode, with an end-of-stream event. At the end, unregisters the
    * listener. Useful if you want to feed the stream to multiple listeners, one at a time. Call the function with the
    * first listener, then the second, etc.
    *
    * @param listener The listener.
    * @return The event stream as an array, typically empty.
    */
  final def emitEventsToListener(listener: StreamListener): EventStream = {
    val registrationId = this.register (listener)
    val es = emitEventsAndClose (EmitMode.ONLINE)
    this.unregister (registrationId)
    es
  }


  /**
    * This is the function that should be implemented by every concrete stream source.
    * If mode is BUFFER, then the source should simply create an array of events and return it as an event stream.
    * If mode is ONLINE, then the source should call stream.source.StreamSource#send2Listeners(stream.GenericEvent) for
    * every event. Returning all events as an event stream array is optional, but not recommended for large streams and
    * NOT FEASIBLE for unbounded streams.
    *
    * @param mode The mode, BUFFER or ONLINE.
    * @return The stream as an array of events.
    */
  protected def emitEvents(mode: EmitMode): EventStream

}
