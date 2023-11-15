package fsm.symbolic

import fsm.symbolic.sre.RegisterVariable
import stream.GenericEvent

object Valuation {
  def apply(v: Map[String,GenericEvent]) = new Valuation(v)

  def apply() = new Valuation(Map.empty)
}

/**
 * Class representing valuations. Used with register automata. It contains a map of register variables (strings) to
 * "stored" events.
 *
 * @param v The map of the valuation.
 */
class Valuation(val v: Map[String,GenericEvent]) extends Serializable {
  /**
   * Creates a new valuation from the current one by adding/updating the new register/event pair.
   *
   * @param registerVariable The variable for the register to be updated.
   * @param newContents The event to be stored.
   * @return The new valuation.
   */
  def update(
              registerVariable: RegisterVariable,
              newContents: GenericEvent
            ): Valuation = update(registerVariable.toString, newContents)

  /**
   * Creates a new valuation from the current one by adding/updating the new register/event pair.
   *
   * @param registerVariableStr The variable for the register to be updated, given as a string.
   * @param newContents The event to be stored.
   * @return The new valuation.
   */
  def update(
              registerVariableStr: String,
              newContents: GenericEvent
            ): Valuation = {
    require(registerVariableStr != "")
    val newRegisters = v.updated(registerVariableStr, newContents)
    Valuation(newRegisters)
  }

  /**
   * Creates a new valuation from the current one by adding/updating the new register/event pair(s).
   * All passed registers will store the same event.
   *
   * @param registerVariablesStr The variables for the registers to be updated, given as a set of strings.
   * @param newContents The event to be stored.
   * @return The new valuation.
   */
  def update(
              registerVariablesStr: Set[String],
              newContents: GenericEvent
            ): Valuation = {
    val newVal = updateAux(registerVariablesStr.toList,this, newContents)
    newVal
  }

  @scala.annotation.tailrec
  private def updateAux(
                         remainingRegisters: List[String],
                         currentValuation: Valuation,
                         newContents: GenericEvent
                       ): Valuation = {
    remainingRegisters match {
      case Nil => currentValuation
      case head::tail => {
        val newValuation = updateVal(currentValuation, head, newContents)
        updateAux(tail, newValuation, newContents)
      }
    }
  }

  def updateVal(
                 valuation: Valuation,
                 registerVariableStr: String,
                 newContents: GenericEvent
               ): Valuation = {
    require(registerVariableStr != "")
    val newRegisters = valuation.v.updated(registerVariableStr, newContents)
    Valuation(newRegisters)
  }

  def compare(other: Valuation): Boolean = v == other.v

  def getRegisterNames: Set[String] = v.keySet

  def hasRegister(r: String): Boolean = v.keySet.contains(r)

  override def toString: String = v.toString()
}
