package stream.source

import stream.array.EventStream
import stream.source.EmitMode.EmitMode

object JsonFileStreamSource {
  def apply(fn: String): JsonFileStreamSource = new JsonFileStreamSource(fn)
}

/**
  * Stream source for JSON files. Every event attribute in the JSON event is mapped to an attribute of the generic
  * event. In BUFFER mode, events are stored in an array of events. In ONLINE mode, events are sent to listeners.
  *
  * @param filename The path to the file.
  */
class JsonFileStreamSource(filename: String) extends StreamSource {

  override protected def emitEvents(
                                     mode: EmitMode,
                                     timeout: Long
                                   ): EventStream = {
    val bufferedSource = io.Source.fromFile(filename)
    var totalCounter = 1
    val eventStream = new EventStream()
    var eventTypes = Set.empty[String]
    for (line <- bufferedSource.getLines) {
      val ge = JsonLineParser.line2Event(line, totalCounter)
      totalCounter += 1
      mode match {
        case EmitMode.BUFFER => {
          eventStream.addEvent(ge)
          eventTypes += ge.eventType
        }
        case EmitMode.ONLINE => send2Listeners(ge)
      }
    }
    bufferedSource.close()
    eventStream.setEventTypes(eventTypes)
    eventStream
  }

}
