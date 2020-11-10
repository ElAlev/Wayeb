package stream.source

import play.api.libs.json.{JsObject, Json}
import stream.GenericEvent

object JsonLineParser extends LineParser {

  override def line2Event(
                           line: String,
                           id: Int
                         ): GenericEvent = {
    val map = Json.parse(line).as[JsObject].value.toMap
    val timestamp = map.getOrElse("timestamp", id).toString.toLong
    GenericEvent(id, "GenericJson", timestamp, map)
  }

  override def line2Event(
                           line: Seq[String],
                           id: Int
                         ): GenericEvent = throw new UnsupportedOperationException("Json domain does not have columns for each line")

}
