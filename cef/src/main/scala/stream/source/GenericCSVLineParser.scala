package stream.source

import com.typesafe.scalalogging.LazyLogging
import stream.{GenericEvent, ResetEvent}

object GenericCSVLineParser extends LineParser with LazyLogging {

  /**
    * First column is the event type. Second column the timestamp.
    *
    * @param line A line, as a sequence of strings.
    * @param id   The new event's unique id.
    * @return The line converted to an event.
    */
  override def line2Event(
                           line: Seq[String],
                           id: Int
                         ): GenericEvent = {
    try {
      val eventType = line.head
      val timestamp = line(1).toLong
      if (timestamp == -1) ResetEvent()
      else {
        val ge = GenericEvent(id, eventType, timestamp)
        ge
      }
    } catch {
      case _: Exception => {
        logger.warn("COULD NOT PARSE LINE " + line)
        throw new Error
      }
    }
  }
}
