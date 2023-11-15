package fsm.runtime

import fsm.symbolic.TransitionOutput.TAKE
import fsm.symbolic.sfa.IdGenerator
import fsm.symbolic.sra.Configuration
import stream.GenericEvent

import scala.collection.mutable

object MatchPool {
  def apply(
             window: Int,
             idg: IdGenerator
           ): MatchPool = new MatchPool(window, idg)

  def apply(idg: IdGenerator): MatchPool = new MatchPool(0, idg)

  def apply(
             window: Int,
             idg: IdGenerator,
             m: Match
           ): MatchPool = {
    val newPool = new MatchPool(window, idg)
    newPool.addMatch(m)
    newPool
  }
}

class MatchPool(
                 window: Int,
                 idg: IdGenerator
               ) {

  private val matches: mutable.Map[Int,Match] = mutable.Map.empty

  private val buffer: mutable.Set[Match] = mutable.Set.empty

  def retrieveNoOfMatches(): Int = matches.size

  def retrieveMatchIds(): Set[Int] = matches.keySet.toSet

  def addMatch(m: Match): Unit = {
    matches += (m.id -> m)
  }

  def addToBuffer(
                   previousMatches: MatchPool,
                   event: GenericEvent,
                   eventCounter: Long,
                   nextConf: Configuration
                 ): Unit = {
    val newClones = previousMatches.matches.filter(x => checkRuntimeWindow(eventCounter, x._2)).values.map(m => m.clone(idg.getIdCautiousMut))
      newClones.foreach(c => {
        if (nextConf.output == TAKE) c.addEvent(event, eventCounter, nextConf)
        buffer += c
      })
  }

  def updateCurrentWithEvent(
                              event: GenericEvent,
                              eventCounter: Long,
                              nextConf: Configuration
                            ): Unit = {
    val toBeRemoved = matches.values.filter(m => !checkRuntimeWindow(eventCounter,m)).map(x => x.id)
    toBeRemoved.foreach(mid => matches.remove(mid))
    matches.values.foreach(m => m.addEvent(event, eventCounter, nextConf))
  }

  def commit(): Unit = {
    buffer.foreach(m => {
      matches += (m.id -> m)
    })
    buffer.clear()
  }

  def removeMatch(m: Match): Unit = {
    matches.remove(m.id)
  }

  def clear(): Unit = {
    matches.clear()
  }

  def nonEmpty: Boolean = matches.nonEmpty

  private def checkRuntimeWindow(
                                  eventCounter: Long,
                                  matchedEvents: Match
                                ): Boolean = {
    // if window = 0, this means that there is no window
    if (window == 0) true
    else {
      val minCounter = matchedEvents.getMinCounter
      // if there are no events in the match (minCounter = -1), the window constraint cannot be violated
      if (minCounter == -1) true
      else {
        // the window constraint is violated if the window value is smaller than the interval spanned by the matched
        // events
        val diff = Math.abs(eventCounter - minCounter)
        (diff < window)
      }
    }
  }

}
