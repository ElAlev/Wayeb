package fsm.runtime

import scala.collection.mutable.ArrayBuffer

/**
  * A structure to hold full matches. This is a run listener, so it expects to receive messages from a fsm.runtime.Run.
  */
class MatchDump extends RunListener {
  private val matches = ArrayBuffer[Match]()

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
   * Checks whether these matches are equivalent to those of another dump.
   * This dump is assumed to have been produced by a deterministic automaton.
   * The other dump is assumed to have been produced by a non-deterministic automaton.
   * The two dumps are equivalent if, for every deterministic match, there exists another non-deterministic match ending
   * at the same point and the events of the non-deterministic dump are a subset of those of the deterministic one.
   * For every non-deterministic match, there must exist a deterministic one ending at the same point and the events of
   * the non-deterministic dump are a subset of those of the deterministic one.
   *
   * @param otherMatchDump The other, non-deterministic dump to check.
   * @return True if the two matches are equivalent.
   */
  def checkAgainstNonDet(otherMatchDump: MatchDump): (Boolean, MatchDump, MatchDump) = {
    var same = true
    val nonDetMatches = otherMatchDump.getMatches
    val orphansNonDet = new MatchDump
    val orphansDet = new MatchDump

    for (ind <- nonDetMatches.indices) {
      var hasEquivalentDet = false
      for (jd <- matches.indices) {
        if (matches(jd).checkDetAgainstNonDet(nonDetMatches(ind))) {
          hasEquivalentDet = true
        }
      }
      if (!hasEquivalentDet) {
        same = false
        val orphanNonDet = nonDetMatches(ind)
        orphansNonDet.addMatch(orphanNonDet)
      }
    }

    for (id <- matches.indices) {
      var hasEquivalentNonDet = false
      for (jnd <- nonDetMatches.indices) {
        if (nonDetMatches(jnd).checkDetAgainstNonDet(matches(id))) {
          hasEquivalentNonDet = true
        }
      }
      if (!hasEquivalentNonDet) {
        same = false
        val orphanDet = matches(id)
        orphansDet.addMatch(orphanDet)
      }
    }

    (same, orphansDet, orphansNonDet)
  }

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

  def toStringWithConfs: String = {
    var s = ""
    for (i <- matches.indices) {
      s += matches(i).toStringWithConfs + "\n"
    }
    s
  }

}
