package stream.array

import java.io.File

import com.github.tototoshi.csv._
import model.markov.TransitionProbs
import stream.GenericEvent
import ui.ConfigUtils

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
//import streamer.FStreamer

object EventStream {

  /**
    * Constructor for an empty event stream.
    *
    * @return An empty stream.
    */
  def apply(): EventStream = new EventStream()

  /**
    * Constructor for event stream from an array buffer of events.
    *
    * @param buffer The buffer of events.
    * @return The event stream.
    */
  def apply(buffer: ArrayBuffer[GenericEvent]): EventStream = {
    val newStream = new EventStream()
    newStream.setStream(buffer)
    newStream
  }
}

/**
  * Event stream represented as an array buffer of events. Usually constructed incrementally, e.g., as we read lines
  * from a file, we convert them to events and add them to the stream.
  */
class EventStream {
  private var eventStream = ArrayBuffer[GenericEvent]()
  private var cumulativeProbs = Map.empty[String, Double]
  private var probs: TransitionProbs = _
  private var eventTypes = Set.empty[String]
  private var counters = scala.collection.immutable.Map.empty[String, Int]
  //private var fStreamer: FStreamer[GenericEvent] = null
  //private var inext = -1

  /**
    * Adds a new event to the stream.
    *
    * @param event The new event.
    */
  def addEvent(event: GenericEvent): Unit = eventStream += event

  /**
    * @return The stream as an array buffer of events.
    */
  def getStream: ArrayBuffer[GenericEvent] = {
    eventStream
    //if (fStreamer == null) eventStream
    //else getStreamFromStreamer
  }

  /**
    * Sets the buffer of events. Previous buffer overwritten.
    *
    * @param buffer The new buffer.
    */
  def setStream(buffer: ArrayBuffer[GenericEvent]): Unit = eventStream = buffer

  /**
    * @return The number of events in the stream.
    */
  def getSize: Int = eventStream.size

  /**
    * @return True if the stream is empty.
    */
  def isEmpty: Boolean = eventStream.isEmpty

  /**
    * Retrieves an event from the stream at a given index.
    *
    * @param index The given index.
    * @return The event at the given index.
    */
  def getEvent(index: Int): GenericEvent = eventStream(index)

  /**
    * Removes all events from the buffer.
    */
  def clearStream(): Unit = eventStream.clear()

  /**
    * Sets the event types to be encountered in the stream.
    *
    * @param et The set of event types.
    */
  def setEventTypes(et: Set[String]): Unit = eventTypes = et

  /**
    * @return All event types to be ecnountered in the stream.
    */
  def getEventTypes: Set[String] = eventTypes

  /**
    * Sets the event type counters (number of events per event type).
    *
    * @param c The counters.
    */
  def setCounters(c: scala.collection.immutable.Map[String, Int]): Unit = counters = c

  /**
    * @return The event counters.
    */
  def getCounters: scala.collection.immutable.Map[String, Int] = counters

  /**
    * Sets the conditional probabilities of the event types.
    *
    * @param p The probabilities.
    */
  def setProbs(p: TransitionProbs): Unit = probs = p

  /**
    * Sets the event type probabilities.
    *
    * @param p The event type probabilities.
    */
  def setProbs(p: mutable.Map[String, Double]): Unit = {
    val tp = new TransitionProbs
    for ((k, v) <- p) {
      tp.addProb(List(k), v)
      tp.addMarginal(k, v)
    }
    probs = tp
  }

  /**
    * Sets the cumulative probabilities for event types.
    *
    * @param p The cumulative event type distribution.
    */
  def setCumulativeProbs(p: mutable.Map[String, Double]): Unit = cumulativeProbs = p

  /**
    * @return The (conditional) event type probabilities.
    */
  def getProbs: TransitionProbs = probs

  /**
    * Splits the stream into substreams according to the values oa a given partition attribute.
    *
    * @param partitionAttribute The given partition attribute.
    * @return The substreams, one per partition value.
    */
  def partitionByAttribute(partitionAttribute: String): mutable.Map[String, EventStream] = {
    var partStreams: mutable.Map[String, EventStream] = mutable.Map.empty
    val singlePartitionVal = ConfigUtils.singlePartitionVal
    for (i <- eventStream.indices) {
      val ev = eventStream(i)
      val av =
        if (partitionAttribute.equalsIgnoreCase(singlePartitionVal)) singlePartitionVal
        else ev.getValueOf(partitionAttribute).toString
      if (partStreams.contains(av)) partStreams(av).addEvent(ev)
      else {
        val newPartStream = EventStream()
        newPartStream.addEvent(ev)
        partStreams += (av -> newPartStream)
      }
    }
    partStreams
  }

  /**
    * Splits a stream into two substreams. The first one has perc % of the initial stream starting from the first event.
    * The second one has the remaining events.
    *
    * @param perc The percentage of events to retain for the first substream.
    * @return The two substreams.
    */
  def split(perc: Double): (EventStream, EventStream) = {
    require(perc > 0.0 & perc <= 0.99)
    val keep = (perc * eventStream.size).toInt
    val trs = new EventStream
    trs.setStream(eventStream.take(keep))
    trs.setProbs(probs)
    trs.setCumulativeProbs(cumulativeProbs)
    trs.setEventTypes(eventTypes)
    val tes = new EventStream
    tes.setStream(eventStream.drop(keep))
    tes.setProbs(probs)
    tes.setCumulativeProbs(cumulativeProbs)
    tes.setEventTypes(eventTypes)
    (trs, tes)
  }

  /**
    * Splits a stream into two substreams. The first one has perc % of the initial stream starting from a given offset.
    * The second one has the remaining events.
    *
    * @param perc The percentage of events to retain for the first substream.
    * @param offset The offset from which the slice of the first substream is to begin.
    * @return The two substreams.
    */
  def split(
             perc: Double,
             offset: Int
           ): (EventStream, EventStream) = {
    require(perc > 0.0 & perc < 0.5)
    val keep = (perc * eventStream.size).toInt
    val realOffset = offset * keep
    require((realOffset + keep) <= eventStream.size)
    val trs = new EventStream
    trs.setStream(eventStream.slice(realOffset, realOffset + keep))
    trs.setProbs(probs)
    trs.setCumulativeProbs(cumulativeProbs)
    trs.setEventTypes(eventTypes)
    val tes = new EventStream
    tes.setStream(eventStream.take(realOffset) ++ eventStream.drop(realOffset + keep))
    tes.setProbs(probs)
    tes.setCumulativeProbs(cumulativeProbs)
    tes.setEventTypes(eventTypes)
    (trs, tes)
  }

  /**
    * Writes the events of the stream in a csv file. Writes only event types and timestamps.
    *
    * @param fn The path to the csv file.
    */
  def writeCSV(fn: String): Unit = {
    val f = new File(fn)
    val writer = CSVWriter.open(f)
    eventStream.foreach(e => writer.writeRow(List(e.eventType, e.timestamp.toString)))
    writer.close()
  }

  /**
    * Writes the events of the stream in a csv file. Writes event types, timestamps and the attributes specified in
    * attributes.
    *
    * @param fn The path to the file.
    * @param attributes The attributes to be written for every event.
    */
  def writeCSV(fn: String, attributes: List[String]): Unit = {
    val f = new File(fn)
    val writer = CSVWriter.open(f)
    eventStream.foreach(e => writer.writeRow(getEventAsStringList(e, attributes, List.empty)))
    writer.close()
  }

  /**
    * @return All events as a list.
    */
  def getEventsAsList: List[GenericEvent] = eventStream.toList

  /**
    * Converts an event to a list of strings.
    *
    * @param e The event.
    * @param attributes The attributes to be retained in the list.
    * @param row The current list.
    * @return The final list of strings.
    */
  @scala.annotation.tailrec
  private def getEventAsStringList(
                                    e: GenericEvent,
                                    attributes: List[String],
                                    row: List[String]
                                  ): List[String] = {
    attributes match {
      case Nil => row.reverse
      case head :: tail => getEventAsStringList(e, tail, e.getValueOf(head).toString :: row)
    }
  }

  /**
    * override toString too expensive (in debug mode, it is by default called)
    *
    * @return The events as a single string.
    */
  def getAsString: String = {
    var s = ""
    for (e <- eventStream) {
      s += "\n" + e.toString()
    }
    s
  }

  /*private def getStreamFromStreamer: ArrayBuffer[GenericEvent] = {
    require(fStreamer != null)
    require(eventStream.isEmpty)
    var e = fStreamer.consumeStream
    while (e != None) {
      addEvent(e.get)
      e = fStreamer.consumeStream
    }
    eventStream
  }

  def setStreamer(s: FStreamer[GenericEvent]): Unit = {fStreamer = s}

  def next: Option[GenericEvent] = {
    val c = if (fStreamer != null) fStreamer.consumeStream
            else {
              inext += 1
              if (inext >= eventStream.size) None
              else Some(getEvent(inext))
            }
    c
  }*/

}
