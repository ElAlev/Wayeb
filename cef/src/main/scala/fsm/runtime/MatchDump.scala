package fsm.runtime

import scala.collection.mutable.ArrayBuffer

/**
  * A structure to hold full matches. This is a run listener, so it expects to receive messages from a fsm.runtime.Run.
  */
class MatchDump extends RunListener {
  private var matches = ArrayBuffer[Match]()

  /**
    * Adds a new match sent from a run.
    * @param rm The message from the run.
    */
  def newEventProcessed(rm: RunMessage): Unit = if (rm.fmDetected) matches += rm.matchedEvents.clone()

  /**
    * Shuts down the dump by clearing all matches.
    */
  override def shutdown(): Unit = {}

  /**
    * Adds a new match.
    * @param m the new match.
    */
  def addMatch(m: Match): Unit = matches += m

  /**
    * Get all stored matches.
    * @return all stored matches.
    */
  def getMatches: ArrayBuffer[Match] = matches

  /**
    * Checks whether these matches are the same as those of another dump.
    * @param otherMatchDump The other dump to check.
    * @return True if this dump contains exactly the same matches as the other dump.
    */
  def checkAgainst(otherMatchDump: MatchDump): Boolean = {
    var same = true
    val againstMatches = otherMatchDump.getMatches

    for (i <- againstMatches.indices) {
      var isContained = false
      for (j <- matches.indices) {
        if (matches(j).checkAgainst(againstMatches(i))) {
          isContained = true
        }
      }
      if (!isContained) {
        same = false
      }
    }

    for (i <- matches.indices) {
      var isContained = false
      for (j <- againstMatches.indices) {
        if (againstMatches(j).checkAgainst(matches(i))) {
          isContained = true
        }
      }
      if (!isContained) {
        same = false
      }
    }

    same
  }

  override def toString: String = {
    var s = ""
    for (i <- matches.indices) {
      s += matches(i).toString() + "\n"
    }
    s
  }

}
