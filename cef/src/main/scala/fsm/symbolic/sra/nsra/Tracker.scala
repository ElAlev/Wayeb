package fsm.symbolic.sra.nsra

import scala.collection.immutable.{Map, Set}

/**
 * Class used to track which states we have checked during elimination.
 */
class Tracker {

  private var statesSeen: Set[Int] = Set[Int]()
  private var statesToSee: Set[Int] = Set[Int]()

  private var id2Set: Map[Int, Set[Int]] = Map[Int, Set[Int]]()
  private var set2Id: Map[Set[Int], Int] = Map[Set[Int], Int]()

  def addSeenState(sd: Int): Unit = {
    require(statesToSee.contains(sd))
    statesSeen = statesSeen + sd
    statesToSee = statesToSee - sd
  }

  def addStateToSee(s: Set[Int]): Int = {
    if (isStateSeen(s)) set2Id(s)
    else if (isStateToSee(s)) set2Id(s)
    else {
      val newId = getNewStateId
      statesToSee = statesToSee + newId
      id2Set += (newId -> s)
      set2Id += (s -> newId)
      newId
    }
  }

  def isStateSeen(s: Set[Int]): Boolean = {
    if (set2Id.contains(s)) statesSeen(set2Id(s))
    else false
  }

  def isStateToSee(s: Set[Int]): Boolean = {
    if (set2Id.contains(s)) statesToSee(set2Id(s))
    else false
  }

  def getNewStateId: Int = statesSeen.size + statesToSee.size

  def hasStatesToSee: Boolean = statesToSee.nonEmpty

  def getStatesToSee: Map[Int, Set[Int]] = {
    var statesToSeeMap = Map[Int, Set[Int]]()
    for (s <- statesToSee) {
      statesToSeeMap += (s -> id2Set(s))
    }
    statesToSeeMap
  }

}
