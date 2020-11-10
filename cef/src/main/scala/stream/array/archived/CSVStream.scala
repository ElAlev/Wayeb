package stream.array.archived

import java.io.File

import com.github.tototoshi.csv.CSVReader
import stream.GenericEvent
import stream.array.{EventStream, EventStreamI}

class CSVStream private[stream] (fn: String) extends EventStreamI {

  def generateStream(): EventStream = {
    val eventStream = new EventStream()
    var counter = 0
    var eventTypes = Set.empty[String]

    val reader = CSVReader.open(new File(fn))
    for (line <- reader) {
      //val eventType: Char = line(0)(0)
      counter += 1
      val ne = createEvent(counter, line)
      eventStream.addEvent(createEvent(counter, line))
      eventTypes += ne.eventType
    }
    reader.close()
    eventStream.setEventTypes(eventTypes)
    eventStream
  }

  def createEvent(id: Int, attributes: Seq[String]): GenericEvent = {
    val eventType: Char = attributes(0)(0)
    val timestamp: Int = attributes(1).toInt
    GenericEvent(id, eventType.toString, timestamp)
  }

}
