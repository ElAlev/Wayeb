package stream.domain.taxi

import stream.GenericEvent
import stream.source.LineParser

import scala.util.matching.Regex

object TaxiLineParser extends LineParser {
  /**
    * Every concrete CSV stream domain must implement this in order to determine how each line is to be converted to an
    * event.
    *
    * @param line A line, as a sequence of strings.
    * @param id   The new event's unique id.
    * @return The line converted to an event.
    */
  override def line2Event(line: Seq[String], id: Int): GenericEvent = {
    val loadPattern = new Regex("TRIP\\(.+")
    val loadMatch = loadPattern.findFirstMatchIn(line.head)
    val ge: GenericEvent = loadMatch match {
      case Some(x) => {
        val eventType = "TRIP"
        //val medallion = line(1).split("=")(1)
        //val hack_license = line(2).split("=")(1)
        //val pickup_datetime = line(3).split("=")(1)
        val dropoff_datetime = line(4).split("=")(1).toLong
        //val trip_time_in_secs = line(5).split("=")(1).toInt
        //val trip_distance = line(6).split("=")(1).toDouble
        val pickup_zone = line(7).split("=")(1).replaceAll("\\s","").replaceAll("/","")
        val dropoff_zone = line(8).split("=")(1).replaceAll("\\s","").replaceAll("/","")
        //val payment_type = line(9).split("=")(1)
        //val fare_amount = line(10).split("=")(1).toDouble
        //val surcharge = line(11).split("=")(1).toDouble
        //val mta_tax = line(12).split("=")(1).toDouble
        //val tip_amount = line(13).split("=")(1).toDouble
        //val tolls_amount = line(14).split("=")(1).toDouble
        val total_amount = line(15).split("=")(1).dropRight(1).toDouble
        val eventId = line.head.split("\\(")(1).split("=")(1).toInt
        val timestamp = dropoff_datetime
        GenericEvent(eventId, eventType, timestamp, Map("pickupZone" -> pickup_zone, "dropoffZone" -> dropoff_zone, "totalAmount" -> total_amount))
      }
      case _ => {
        throw new Exception("COULD NOT PARSE LINE")
      }
    }
    ge

  }
}
