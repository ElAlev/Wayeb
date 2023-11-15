package fsm.runtime

import fsm.WindowType.{COUNT, WindowType}
import stream.GenericEvent

object MatchList {
  def apply(): MatchList = new MatchList(List.empty, -1, -1, false)

  def apply(
             initialMatch: List[Int],
             inititalMinCounter: Long,
             inititalMaxCounter: Long,
             initialFull: Boolean
           ): MatchList = new MatchList(initialMatch, inititalMinCounter, inititalMaxCounter, initialFull)

}



class MatchList(
                 val initialMatch: List[Int],
                 val inititalMinCounter: Long,
                 val inititalMaxCounter: Long,
                 val initialFull: Boolean
               ) {

  private var events: List[Int] = initialMatch
  private var full = initialFull
  // the minimum and maximum event counters among all the events stored in the match
  private var minCounter: Long = inititalMinCounter
  private var maxCounter: Long = inititalMaxCounter

  def addEvent(
                event: GenericEvent,
                counter: Long,
                windowType: WindowType
              ): Unit = {
    events = event.id :: events
    if (windowType == COUNT) updateCounters(counter) else updateCounters(event.timestamp)
  }

  private def updateCounters(newCounter: Long): Unit = {
    //require(newCounter > 0)
    if (minCounter == -1 | newCounter < minCounter) minCounter = newCounter
    //if (maxCounter == -1 | newCounter > maxCounter) maxCounter = newCounter
  }

  def getEvents: List[Int] = events

  def getMinCounter: Long = minCounter

  def getMaxCounter: Long = maxCounter

  /**
   * Sets the match to be full or partial.
   *
   * @param f If true, the match is full.
   */
  def setFull(f: Boolean): Unit = full = f

  /**
   * Checks whether the match is full.
   *
   * @return True if it is a full match.
   */
  def isFull: Boolean = full

  /**
   * Clears the match from all events.
   */
  def clear(): Unit = {
    events = List.empty
    full = false
  }

  private def setMinCounter(mc: Long): Unit = minCounter = mc

  private def setMaxCounter(mc: Long): Unit = maxCounter = mc


  override def clone(): MatchList = {
    val newMatch = MatchList(events, minCounter, maxCounter, full)
    newMatch
  }

  override def toString: String = {
    var s = ""
    if (events.nonEmpty) {
      for (e <- events.reverse) {
        s += e + "->"
      }
      if (full) s += "COMPLETE"
      else s += "PARTIAL"
    }
    s
  }


}
