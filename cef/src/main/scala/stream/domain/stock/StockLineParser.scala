package stream.domain.stock

import stream.GenericEvent
import stream.source.LineParser

import scala.util.matching.Regex

object StockLineParser extends LineParser {
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
    val buyTypePattern = new Regex("BUY\\(.+")
    val sellTypePattern = new Regex("SELL\\(.+")
    val buyEventMatch = buyTypePattern.findFirstMatchIn(line.head)
    val ge: GenericEvent = buyEventMatch match {
      case Some(x) => {
        val eventType = "BUY"
        val name = line(1).split("=")(1)
        val volume = line(2).split("=")(1).toInt
        val price = line(3).split("=")(1).toDouble
        val eventId = line.head.split("\\(")(1).split("=")(1).toInt
        val timestamp = line(4).split("=")(1).dropRight(1).toLong
        GenericEvent(eventId, eventType, timestamp, Map("name" -> name, "volume" -> volume, "price" -> price))
      }
      case _ => {
        val sellEventMatch = sellTypePattern.findFirstMatchIn(line.head)
        sellEventMatch match {
          case Some(y) => {
            val eventType = "SELL"
            val name = line(1).split("=")(1)
            val volume = line(2).split("=")(1).toInt
            val price = line(3).split("=")(1).toDouble
            val eventId = line.head.split("\\(")(1).split("=")(1).toInt
            val timestamp = line(4).split("=")(1).dropRight(1).toLong
            GenericEvent(eventId, eventType, timestamp, Map("name" -> name, "volume" -> volume, "price" -> price))
          }
          case _ => {
            throw new Exception("COULD NOT PARSE STOCK EVENT")
          }
        }
      }
    }
    ge
  }
}
