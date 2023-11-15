package fsm.runtime

import fsm.WindowType.{COUNT, WindowType}
import fsm.symbolic.sra.Configuration
import stream.GenericEvent

import scala.collection.mutable.ListBuffer

object Match {
  def apply(id: Int): Match = new Match(id)

  def apply(): Match = new Match(0)
}
/**
  * Class representing matches. Just a list with info from the events of a match.
  */
class Match(val id: Int) {
  // For each event, we store its id, the event counter (fsm.runtime.Run) and the automaton configuration.
  private val events = ListBuffer[(Int, Long, Configuration)]()
  private val eventsSimple = ListBuffer[(Int, Long)]()
  // if full=true, means that the match is a full match, otherwise it's a partial match
  private var full = false
  // the minimum and maximum event counters among all the events stored in the match
  private var minCounter: Long = -1
  private var maxCounter: Long = -1


  def addEvent(
                event: GenericEvent,
                counter: Long
              ): Unit = {
    val newRecord: (Int, Long) = (event.id, counter)
    eventsSimple += newRecord
    updateCounters(counter)
  }

  /**
    * Adds a new event to the match.
    * @param event The event to be added.
    * @param counter The event counter.
    * @param conf The automaton configuration.
    * @param windowType The window type.
    */
  def addEvent(
                event: GenericEvent,
                counter: Long,
                conf: Configuration,
                windowType: WindowType
              ): Unit = {
    val newRecord: (Int, Long, Configuration) = (event.id, counter, conf)
    events += newRecord
    if (windowType == COUNT) updateCounters(counter) else updateCounters(event.timestamp)
  }

  /**
   * Adds a new event to the match.
   *
   * @param event   The event to be added.
   * @param counter The event counter.
   * @param conf    The automaton configuration.
   */
  def addEvent(
                event: GenericEvent,
                counter: Long,
                conf: Configuration
              ): Unit = {
    val newRecord: (Int, Long, Configuration) = (event.id, counter, conf)
    events += newRecord
    updateCounters(counter)
  }

  /**
    * Adds a new event to the match.
    * @param eventId The id of the event to be added.
    * @param counter The event counter.
    * @param conf The automaton configuration.
    */
  private def addEvent(
                        eventId: Int,
                        counter: Long,
                        conf: Configuration
                      ): Unit = {
    val newRecord: (Int, Long, Configuration) = (eventId, counter, conf)
    events += newRecord
    updateCounters(counter)
  }

  /**
    * Updates the min/max counters.
    *
    * @param newCounter The new event counter.
    */
  private def updateCounters(newCounter: Long): Unit = {
    require(newCounter > 0)
    if ( minCounter == -1 | newCounter < minCounter ) minCounter = newCounter
    if ( maxCounter == -1 | newCounter > maxCounter ) maxCounter = newCounter
  }

  def getMinCounter: Long = minCounter

  def getMaxCounter: Long = maxCounter

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

  private def setMinCounter(mc: Long): Unit = minCounter = mc

  private def setMaxCounter(mc: Long): Unit = maxCounter = mc

  /**
    * Clones the match.
    * @return A match with the same events.
    */
  override def clone(): Match = {
    val newMatch = Match()
    for (e <- events) {
      newMatch.addEvent(e._1, e._2, e._3)
    }
    newMatch.setFull(full)
    newMatch.setMinCounter(this.minCounter)
    newMatch.setMaxCounter(this.maxCounter)
    newMatch
  }

  def clone(id: Int): Match = {
    val newMatch = Match(id)
    for (e <- events) {
      newMatch.addEvent(e._1, e._2, e._3)
    }
    newMatch.setFull(full)
    newMatch.setMinCounter(this.minCounter)
    newMatch.setMaxCounter(this.maxCounter)
    newMatch
  }

  /**
    * Adds a list of events.
    * @param eventIds The ids of the events.
    */
  def setEvents(eventIds: ListBuffer[Int]): Unit = {
    events.clear()
    for (e <- eventIds) {
      val newRecord = (e, 0.toLong, Configuration(0))
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
  def getEventsWithInfo: ListBuffer[(Int, Long, Configuration)] = events

  /**
    * Checks whether this match is the same as another match. The event ids and the full flag must be the same.
    * @param otherMatch The other match against which we check this match.
    * @return True if the matches are the same.
    */
  def checkAgainst(otherMatch: Match): Boolean = (full == otherMatch.isFull) & (this.getEvents == otherMatch.getEvents)

  /**
   * Checks whether this match is equivalent to another match.
   * This match is assumed to be deterministic. The other non-deterministic.
   * The full flags must be the same and the deterministic match must have the same maxCounter as the non-deterministic.
   *
   * @param otherMatch The other match against which we check this match.
   * @return True if the matches are equivalent.
   */
  def checkDetAgainstNonDet(otherMatch: Match): Boolean = (full == otherMatch.isFull) & (otherMatch.getMaxCounter == this.getMaxCounter) //(otherMatch.getEvents.forall(e => this.getEvents.contains(e)))

  /**
   * Checks whether this match is equivalent to another match.
   * This match is assumed to be non-deterministic. The other deterministic.
   * The full flags must be the same and the deterministic match must contain all the events of the non-deterministic.
   *
   * @param otherMatch The other match against which we check this match.
   * @return True if the matches are equivalent.
   */
  def checkNonDetAgainstDet(otherMatch: Match): Boolean = (full == otherMatch.isFull) & (this.getEvents.forall(e => otherMatch.getEvents.contains(e)))

  def toStringWithConfs: String = {
    var s = ""
    if (events.nonEmpty) {
      for (e <- events) {
        s += e._1 + "{" + e._3 + "}->"
      }
      if (full) s += "COMPLETE"
      else s += "PARTIAL"
    }
    s
  }

  override def toString: String = {
    var s = ""
    if (events.nonEmpty) {
      for (e <- events) {
        s += e._1 + "->"
      }
      if (full) s += "COMPLETE"
      else s += "PARTIAL"
    }
    s

    /*if (events.nonEmpty) {
      if (full) events.head._1 + "->...->" + events.last._1 + "->COMPLETE"
      else events.head._1 + "->...->" + events.last._1 + "->PARTIAL"
    }
    else ""*/

  }

}
