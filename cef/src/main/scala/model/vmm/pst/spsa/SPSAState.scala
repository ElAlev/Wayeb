package model.vmm.pst.spsa

import model.vmm.{Symbol, SymbolWord}
import scala.collection.SortedMap

object SPSAState {
  /**
    * Constructor for PSA states.
    *
    * @param sfaId The SDFA state id.
    * @param psaLabel The PSA label.
    * @param tr The state's outgoing transitions, as a map of symbols to transtions.
    * @param isStart Indicates whether the state is a start state.
    * @param isFinal Indicates whether the state is a final state.
    * @return The SPSA state.
    */
  def apply(
             sfaId: Int,
             psaLabel: SymbolWord,
             tr: Map[Symbol, SPSATransition],
             isStart: Boolean,
             isFinal: Boolean
           ): SPSAState = new SPSAState(sfaId, psaLabel, tr, isStart, isFinal)

  /**
    * Constructor for PSA states. In case no transitions are provided, create a state with no outgoing transitions.
    *
    * @param sfaId The SDFA state id.
    * @param psaLabel The PSA label.
    * @param isStart Indicates whether the state is a start state.
    * @param isFinal Indicates whether the state is a final state.
    * @return The SPSA state.
    */
  def apply(
             sfaId: Int,
             psaLabel: SymbolWord,
             isStart: Boolean,
             isFinal: Boolean
           ): SPSAState = new SPSAState(sfaId, psaLabel, Map.empty, isStart, isFinal)

  /**
    * Constructor for PSA states. In case no transition and no information about the state type are provided , create a
    * state with no outgoing transitions and set this state as being neither a start nor a final state.
    *
    * @param sfaId The SDFA state id.
    * @param psaLabel The PSA label.
    * @return The SPSA state.
    */
  def apply(
             sfaId: Int,
             psaLabel: SymbolWord
           ): SPSAState = new SPSAState(sfaId, psaLabel, Map.empty, false, false)
}

/**
  * This class represents a state of a SPSA, i.e., an embedding of a PSA in a SDFA.
  * Each SPSA state is constructed by combining a state from the SDFA with a state from the PSA.
  * As a result, each SPSA state has the id of the parent SDFA state (Int) and the label of the PSA state
  * (word of symbols). Thus, the id of a SPSA state is the SDFA state id plus the PSA state label.
  *
  * @param sfaId The SDFA state id.
  * @param psaLabel The PSA label.
  * @param tr The transitions from this state, given as map holding a transition for every symbol.
  * @param isStart Indicates whether the state is a start state.
  * @param isFinal Indicates whether the state is a final state.
  */
class SPSAState(
                 val sfaId: Int,
                 val psaLabel: SymbolWord,
                 tr: Map[Symbol, SPSATransition],
                 val isStart: Boolean,
                 val isFinal: Boolean
               ) extends Serializable {
  private var transitions = tr

  /**
    * Adds a new target state by creating a new transition.
    *
    * @param nextState The target state.
    * @param withSymbol The transition symbol.
    * @param prob The transition probability.
    */
  def addNextState(
                    nextState: SPSAState,
                    withSymbol: Symbol,
                    prob: Double
                  ): Unit = {
    val newTransition = SPSATransition(nextState, withSymbol, prob)
    transitions = transitions + (withSymbol -> newTransition)
  }

  /**
    * Returns the target state for a given symbol.
    * Assumes that the state does indeed have a transition with the given symbol.
    *
    * @param withSymbol The given symbol
    * @return The target state.
    */
  def delta(withSymbol: Symbol): SPSAState = {
    require(transitions.contains(withSymbol))
    transitions(withSymbol).target
  }

  /**
    * Returns true if the state has a target state with the given id/label.
    *
    * @param to The id/label tuple.
    * @return True if a target state exists.
    */
  def connected(to: (Int, SymbolWord)): Boolean = {
    transitions.values.exists(t => t.target.sfaId == to._1 & t.target.psaLabel == to._2)
  }

  /**
    * Returns the transition probability for a given symbol (or 0 if no transition with the symbol exists).
    *
    * @param symbol The given symbol.
    * @return The transition probability.
    */
  def getProbFor(symbol: Symbol): Double = {
    if (transitions.contains(symbol)) transitions(symbol).prob
    else 0.0
  }

  /**
    * Returns the transition probability to a state (or 0 if no transition with exists).
    *
    * @param label The id/label of the candidate target state.
    * @return The transition probability.
    */
  def getProbToState(label: (Int, SymbolWord)): Double = {
    val transition = transitions.find(t => t._2.target.sfaId == label._1 & t._2.target.psaLabel == label._2)
    transition match {
      case Some(t) => t._2.prob
      case None => 0.0
    }
  }

  /**
    * @return All outgoing transitions as map of symbols to SPSA transitions.
    */
  def getTransitions: Map[Symbol, SPSATransition] = transitions

  /**
    * @return All outgoing transitions as map of symbols to SPSA transitions, sorted by symbol.
    */
  private def getTransitionsSortedBySymbol: SortedMap[Symbol, SPSATransition] = {
    val sorted = SortedMap(transitions.toSeq: _*)
    sorted
  }

  /**
    * @return All transitions as a string, sorted by symbol.
    */
  private def transitions2str: String = {
    val sortedTransitions = getTransitionsSortedBySymbol
    sortedTransitions.foldLeft("")((x, y) => x + "\n" + y._2.toString)
  }

  override def toString: String = "Label: " + psaLabel + "\n" + transitions2str

}
