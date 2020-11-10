package stream.source

import stream.array.EventStream
import stream.source.EmitMode.EmitMode

object ArrayStreamSource {
  def apply(eventStream: EventStream): ArrayStreamSource = new ArrayStreamSource(eventStream)
}

/**
  * A stream source created from an already existing stream array.
  *
  * @param eventStream The stream array of events.
  */
class ArrayStreamSource(eventStream: EventStream) extends StreamSource {

  /**
    * For BUFFER mode, simply return the array. For ONLINE, send all events to listeners.
    *
    * @param mode The mode, BUFFER or ONLINE.
    * @return The stream as an array of events.
    */
  override protected def emitEvents(mode: EmitMode): EventStream = {
    mode match {
      case EmitMode.BUFFER => eventStream
      case EmitMode.ONLINE => {
        val streamSize = eventStream.getSize
        for (i <- 0 until streamSize) {
          val event = eventStream.getEvent(i)
          send2Listeners(event)
        }
        eventStream
      }
    }
  }
}
