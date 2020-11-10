package fsm.classical.fa.nfa

import scala.collection.mutable

/**
  * Class representing the state of a NFA.
  *
  * @param i The unique id of the state.
  */
class NFAState private[nfa] (i: Int) {
  private var id = i
  private var delta = mutable.Map[String, mutable.Set[Int]]()
  private var isStart = false
  private var isAccepting = false

  /**
    * Adds a new outgoing transition to the state.
    *
    * @param symbol The transition symbol. Other transitions with the same symbol might already exist. They are not
    *               overwritten.
    * @param nextStateId The id of the next state.
    */
  def addDelta(
                symbol: String,
                nextStateId: Int
              ): Unit = {
    if (delta.contains(symbol)) {
      val nextSet = delta(symbol)
      if (!nextSet.contains(nextStateId)) {
        nextSet += nextStateId
        delta(symbol) = nextSet
      }

    } else {
      val newNextSet = mutable.Set(nextStateId)
      delta += (symbol -> newNextSet)
    }
  }

  /**
    * Retrieves the state we can reach with a given symbol. If no transitions with the given symbol exist, an empty set
    * is returned.
    *
    * @param symbol The given symbol
    * @return The states reached, given as a set of ids.
    */
  def getDelta(symbol: String): mutable.Set[Int] = {
    if (delta.contains(symbol)) delta(symbol)
    else mutable.Set[Int]()
  }

  /**
    * Deletes all outgoing transitions.
    */
  def clearDelta(): Unit = delta.clear()

  /**
    * @return The ids of all states we can reach, regardless of the symbol.
    */
  def getAllNext: mutable.Set[Int] = delta.foldLeft(mutable.Set[Int]())((x, y) => x ++ y._2)

  /**
    * Checks whether there exists a transition with the given symbol.
    *
    * @param symbol The given symbol.
    * @return True if there exists a transitions with the given symbol.
    */
  def hasSymbol(symbol: String): Boolean = delta.contains(symbol)

  /**
    * Sets the state as start or not.
    *
    * @param is If true, the state is set as start.
    */
  def setAsStart(is: Boolean): Unit = isStart = is

  /**
    * Sets the state as final or not.
    *
    * @param ia If true, the state is set as final.
    */
  def setAsAccepting(ia: Boolean): Unit = isAccepting = ia

  override def toString: String = {
    var s = ""
    for ((k, v) <- delta) {
      s += "\n\t" + k + "\t|\t" + v
    }
    s
  }
}
