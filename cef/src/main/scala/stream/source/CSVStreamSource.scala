package stream.source

import com.github.tototoshi.csv.CSVReader
import stream.GenericEvent
import stream.array.EventStream
import stream.source.EmitMode.EmitMode

object CSVStreamSource {

  /**
    *
    * @param filename The path to the file.
    * @param domain The domain of the source
    * @return The stream source.
    */
  def apply(
             filename: String,
             domain: LineParser
           ): CSVStreamSource = new CSVStreamSource(filename, domain)

  /**
    *
    * @param filename The path to the file.
    * @return The stream source.
    */
  def apply(filename: String): CSVStreamSource = new CSVStreamSource(filename, GenericCSVLineParser)
}


/**
  * Stream source for CSV files.
  *
  * @param filename The path to the file.
  */
class CSVStreamSource(
                       filename: String,
                       domain: LineParser
                     ) extends StreamSource {

  /**
    * After reading every line, it either sends it (as an event) to the listeners if in ONLINE mode or stores it to an
    * event stream if in BUFFER mode.
    *
    * @param mode The mode, BUFFER or ONLINE.
    * @return The stream as an array of events.
    */
  override def emitEvents(mode: EmitMode): EventStream = {
    val eventStream = new EventStream()
    var totalCounter = 1
    var eventTypes = Set.empty[String]
    val reader = CSVReader.open(filename)
    val it = reader.iterator
    while (it.hasNext) {
      val line = it.next()
      val newEvent = domain.line2Event(line, totalCounter)
      totalCounter += 1
      mode match {
        case EmitMode.BUFFER => {
          eventStream.addEvent(newEvent)
          eventTypes += newEvent.eventType
        }
        case EmitMode.ONLINE => send2Listeners(newEvent)
      }
    }
    reader.close()
    eventStream.setEventTypes(eventTypes)
    eventStream
  }

}
