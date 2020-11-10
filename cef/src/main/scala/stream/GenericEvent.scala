package stream

object GenericEvent {
  /**
    * Constructor for generic events.
    *
    * @param id The unique id.
    * @param eventType The event type.
    * @param timestamp The timestamp.
    * @param extraArgs The map of extra attributes, if any. Could be empty.
    * @return The event.
    */
  def apply(
      id: Int,
      eventType: String,
      timestamp: Long,
      extraArgs: Map[String, Any]
  ): GenericEvent =
    new GenericEvent(id, eventType, timestamp, extraArgs)

  /**
    * Constructor for generic events without any extra attributes.
    *
    * @param id The unique id.
    * @param eventType The event type.
    * @param timestamp The timestamp.
    * @return The event.
    */
  def apply(
      id: Int,
      eventType: String,
      timestamp: Long
  ): GenericEvent =
    new GenericEvent(id, eventType, timestamp, Map[String, Any]())

  /**
    * Constructor for generic events without any extra attributes.
    * the timestamp acts as the id.
    *
    * @param eventType The event type.
    * @param timestamp The timestamp.
    * @return The event.
    */
  def apply(
      eventType: String,
      timestamp: Long
  ): GenericEvent =
    new GenericEvent(timestamp.toInt, eventType, timestamp, Map[String, Any]())
}

/**
  * Each event has a unique ed, an event type and a timestamp. Any extra attributes are represented as a map.
  *
  * @param id The unique id.
  * @param eventType The event type.
  * @param timestamp The timestamp.
  * @param extraArgs The map of extra attributes, if any. Could be empty.
  */
class GenericEvent(
                    val id: Int,
                    val eventType: String,
                    val timestamp: Long,
                    extraArgs: Map[String, Any]
                  ) {

  /**
    * Checks if the event has a specific extra attribute (besides id, type and timestamp).
    *
    * @param attribute The attribute to check.
    * @return True if the event has this attribute.
    */
  def hasAttribute(attribute: String): Boolean = extraArgs.contains(attribute)

  /**
    * Retrieves the value of an extra attribute. Assumes the event has such an attribute.
    *
    * @param attribute The attribute.
    * @return The attribute's value.
    */
  def getValueOfMap(attribute: String): Any = extraArgs(attribute)

  /**
    * Retrieves the value of an attribute. Assumes the event has such an attribute.
    *
    * @param attribute The attribute.
    * @return The attribute's value.
    */
  def getValueOf(attribute: String): Any = {
    attribute match {
      case "Id" => id
      case "EventType" => eventType
      case "Timestamp" => timestamp
      case _ => getValueOfMap(attribute)
    }
  }

  override def toString: String =
    id + "\t|\t" + timestamp + "\t|\t" + eventType + "\t|\t" + extraArgs.toString()
}
