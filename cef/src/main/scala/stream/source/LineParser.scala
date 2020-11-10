package stream.source

import stream.GenericEvent

/**
  * Since for different domains we might need to parse lines differently, for each domain (when not JSON) we need to
  * create a class inheriting from this class and implement its methods.
  */
abstract class LineParser(delimiter: String = ",") {

  /**
    * Every concrete CSV stream domain must implement this in order to determine how each line is to be converted to an
    * event.
    *
    * @param line A line, as a sequence of strings.
    * @param id The new event's unique id.
    * @return The line converted to an event.
    */
  def line2Event(
                  line: Seq[String],
                  id: Int
                ): GenericEvent

  /**
    * In case the input is just a single string, we first break it into a sequence of strings and then call
    * stream.domain.Domain#line2Event(java.lang.String, int, java.lang.String).
    *
    * @param line A line, as a string.
    * @param id The new event's unique id.
    * @return The line converted to an event.
    */
  def line2Event(
                  line: String,
                  id: Int
                ): GenericEvent = {
    line2Event(line.split(delimiter), id)
  }

}
