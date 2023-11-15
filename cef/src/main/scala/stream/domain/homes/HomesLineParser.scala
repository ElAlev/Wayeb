package stream.domain.homes

import stream.GenericEvent
import stream.source.LineParser

import scala.util.matching.Regex

object HomesLineParser extends LineParser {
  /**
   * Every concrete CSV stream domain must implement this in order to determine how each line is to be converted to an
   * event.
   *
   * @param line A line, as a sequence of strings.
   * @param id   The new event's unique id.
   * @return The line converted to an event.
   */
  override def line2Event(
                           line: Seq[String],
                           id: Int
                         ): GenericEvent = {
    val loadPattern = new Regex("LOAD\\(.+")
    val loadMatch = loadPattern.findFirstMatchIn(line.head)
    val ge: GenericEvent = loadMatch match {
      case Some(x) => {
        val eventType = "LOAD"
        val plug_timestamp = line(1).split("=")(1)
        val value = line(2).split("=")(1).toDouble
        val household_id = line(4).split("=")(1).dropRight(1).toLong
        val eventId = line.head.split("\\(")(1).split("=")(1).toInt
        val timestamp = plug_timestamp.toLong
        GenericEvent(eventId, eventType, timestamp, Map("plug_timestamp" -> plug_timestamp, "value" -> value, "householdId" -> household_id))
      }
      case _ => {
        throw new Exception("COULD NOT PARSE LINE")
      }
    }
    ge

  }
}
