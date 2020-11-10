package fsm.classical.fa.dfa

import fsm.classical.FATransition
import scala.collection.mutable

/**
  * Class representing a DFA state.
  *
  * @param i The unique id of the state.
  */
class DFAState private[dfa] (i: Int) extends Serializable {
  private var id = i
  private var delta = mutable.Map.empty[String, Int]
  private var output = mutable.Set.empty[String]
  private var start = false
  private var semiFinal = false
  private var semiStart = false
  private var order = 0
  private var label: List[String] = List.empty[String]
  private var duplicateOf: Int = -1

  /**
    * @return All transitions.
    */
  def getTransitions: List[FATransition] = delta.map(d => FATransition(id, d._2, d._1)).toList

  /**
    * Sets the delta, the transition function. Overwrites previous.
    *
    * @param d The new delta.
    */
  def setDelta(d: mutable.Map[String, Int]): Unit = {
    delta.clear()
    for ((k, v) <- d) {
      delta += (k -> v)
    }
  }

  /**
    * Updates the transition for the given symbol to point to a new target state. There must already exist a transition
    * with the given symbol.
    *
    * @param symbol The given symbol.
    * @param nextState The new target state.
    */
  def setDelta(
                symbol: String,
                nextState: Int
              ): Unit = {
    require(delta.contains(symbol)) // otherwise, use addDelta
    delta(symbol) = nextState
  }

  /**
    * Sets the order of the state.
    *
    * @param o The order.
    */
  def setOrder(o: Int): Unit = {
    require(order >= 0)
    order = o
  }

  /**
    * Sets the state's label, i.e., the sequence of length order that can lead to it.
    *
    * @param dq The label, as a set. It must be either a singleton set (since this is supposed to be a disambiguated
    *           DFA) or the empty set if this is the start state.
    */
  def setLabel(dq: mutable.Set[List[String]]): Unit = {
    require(dq.size == 1 | (start & dq.isEmpty), "dq: " + dq)
    if (start & dq.isEmpty) label = List.empty[String]
    else setLabel(dq.head)
  }

  /**
    * Sets the state's label. The label's length must be equal to the state's order.
    *
    * @param l The label as a word (list of symbols).
    */
  private def setLabel(l: List[String]): Unit = {
    if (l.size != order)
      throw new IllegalArgumentException("Length of DFA label should be equal to its order: " + label + ".size!=" + order)
    label = l
  }

  /**
    * Adds a new transition to a target state with a given symbol. If there already exists a transition with the same
    * symbol, it will be overwritten.
    *
    * @param symbol The given symbol.
    * @param nextStateId The id of the target state.
    */
  def addDelta(
                symbol: String,
                nextStateId: Int
              ): Unit = delta += (symbol -> nextStateId)

  /**
    * Sets the state;s output.
    *
    * @param o The output.
    */
  def setOutput(o: mutable.Set[String]): Unit = output = o

  /**
    * Sets the state as start or not.
    *
    * @param s If true, the state is set as start.
    */
  def setAsStart(s: Boolean): Unit = start = s

  /**
    * @return True if the state is the start state.
    */
  def isStart: Boolean = start

  /**
    * Finds the target state with a given symbol.
    *
    * @param symbol The given symbol.
    * @return The state reached with the given symbol or -1 if no relevant transition exists.
    */
  def getDelta(symbol: String): Int = {
    if (!delta.contains(symbol)) -1
    else delta(symbol)
  }

  /**
    * @return All transitions.
    */
  def getDeltaFunction: mutable.Map[String, Int] = delta

  /**
    * @return The state's output.
    */
  def getOutput: mutable.Set[String] = output

  /**
    * @return The state's label.
    */
  def getLabel: List[String] = label

  /**
    * @return true if this is a final state.
    */
  def isFinal: Boolean = output.nonEmpty

  /**
    * Sets this state to be a duplicate of the given state.
    *
    * @param d The given state.
    */
  def setAsDuplicateOf(d: Int): Unit = {
    require(d > -1)
    duplicateOf = d
  }

  /**
    * Checks whether the state is a duplicate of some other state.
    *
    * @return True if the state is a duplicate.
    */
  def isDuplicate: Boolean = duplicateOf != -1

  /**
    * Retrieves the id of the original state from which this state was derived (after disambiguation).
    *
    * @return The id of the original state.
    */
  def getOriginal: Int = duplicateOf

  /**
    * Checks whether this state is a duplicate of the given state.
    *
    * @param d The id of the given state.
    * @return True if this state is a duplicate of the given.
    */
  def isDuplicateOf(d: Int): Boolean = duplicateOf == d

  /**
    * @return The delta function of the state as a string.
    */
  def printDelta: String = {
    var s = ""
    for ((k, v) <- delta) {
      s += "\n\t" + k + "\t|\t" + v
    }
    s
  }

  /*
  def setAsSemiStart(s: Boolean): Unit = semiStart = s

  def setAsSemiFinal(s: Boolean): Unit = semiFinal = s

  def isSemiFinal: Boolean = semiFinal

  def isSemiStart: Boolean = semiStart

  def shiftDelta(s: Int): Unit = {
    id += s
    var newDelta = mutable.Map.empty[String, Int]
    for ((k, v) <- delta) {
      newDelta += (k -> (v + s))
    }
    delta = newDelta
  }
   */
}
