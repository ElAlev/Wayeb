package fsm.runtime

import stream.GenericEvent
import scala.collection.mutable.ListBuffer

/**
  * Class representing matches. Just a list with info from the events of a match.
  */
class Match {
  // For each event, we store its id, the event counter (fsm.runtime.Run) and the automaton state.
  private val events = ListBuffer[(Int, Long, Int)]()
  // if full=true, means that the match is a full match, otherwise it's a partial match
  private var full = false

  /**
    * Adds a new event to the match.
    * @param event The event to be added.
    * @param counter The event counter.
    * @param state The automaton state.
    */
  def addEvent(
                event: GenericEvent,
                counter: Long,
                state: Int
              ): Unit = {
    val newRecord = (event.id, counter, state)
    events += newRecord
  }

  /**
    * Adds a new event to the match.
    * @param eventId The id of the event to be added.
    * @param counter The event counter.
    * @param state The automaton state.
    */
  private def addEvent(
                        eventId: Int,
                        counter: Long,
                        state: Int
                      ): Unit = {
    val newRecord = (eventId, counter, state)
    events += newRecord
  }

  /**
    * Sets the match to be full or partial.
    * @param f If true, the match is full.
    */
  def setFull(f: Boolean): Unit = full = f

  /**
    * Checks whether the match is full.
    * @return True if it is a full match.
    */
  def isFull: Boolean = full

  /**
    * Clears the match from all events.
    */
  def clear(): Unit = {
    events.clear()
    full = false
  }

  /**
    * Clones the match.
    * @return A match with the same events.
    */
  override def clone(): Match = {
    val newm = new Match()
    for (e <- events) {
      newm.addEvent(e._1, e._2, e._3)
    }
    newm.setFull(full)
    newm
  }

  /**
    * Adds a list of events.
    * @param eventIds The ids of the events.
    */
  def setEvents(eventIds: ListBuffer[Int]): Unit = {
    events.clear()
    for (e <- eventIds) {
      val newRecord = (e, 0.toLong, 0)
      events += newRecord
    }
  }

  /**
    * Returns the ids of the events of the match.
    * @return The ids of the events of the match
    */
  def getEvents: ListBuffer[Int] = events.map(e => e._1)

  /**
    * Returns events with complete info.
    * @return events with complete info.
    */
  def getEventsWithInfo: ListBuffer[(Int, Long, Int)] = events

  /**
    * Checks whether this match is the same as another match. The event ids and the full flag must be the same.
    * @param otherMatch The other match against which we check this match.
    * @return True if the matches are the same.
    */
  def checkAgainst(otherMatch: Match): Boolean = (full == otherMatch.isFull) & (this.getEvents == otherMatch.getEvents)

  override def toString: String = {
    /*var s = ""
    for (e <- events) {
      s += e + "->"
    }
    if (full) s += "COMPLETE"
    else s += "PARTIAL"
    s*/
    if (full) events.head._1 + "->...->" + events.last._1 + "->COMPLETE"
    else events.head._1 + "->...->" + events.last._1 + "->PARTIAL"
  }

}
