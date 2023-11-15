package fsm.symbolic.sfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.Automaton
import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.symbolic.sra.Configuration
import stream.GenericEvent

/**
  * Abstract class representing symbolic automata.
  *
  * @param states The states of the automaton as a map of IDs to states.
  * @param transitions The list of transitions.
  * @param start The id of the start state.
  * @param finals The set of IDs of the final states.
  */
abstract class SFA(
                    states: Map[Int, SFAState],
                    transitions: List[SFATransition],
                    start: Int,
                    finals: Set[Int]
                  ) extends Automaton(states, transitions, start, finals) with Serializable with LazyLogging {
  require(start >= 0)
  require(states.keySet.contains(start))
  require(finals.forall(f => states.keySet.contains(f)))


  /**
    * Finds the state(s) we can reach from a given state with a given event.
    *
    * @param fromStateId The id of the given state.
    * @param withEvent The given event.
    * @return The state(s) reached.
    */
  override def getDelta(
                         fromStateId: Int,
                         withEvent: GenericEvent
                       ): Set[Configuration] = {
    val result = super.getDelta(fromStateId, withEvent)
    if (this.isInstanceOf[SDFA] & result.size != 1) {
      logger.error("Delta for SDFA should always return single state.")
      throw new Error("Delta for SDFA should always return single state.")
    }
    result
  }

  override def getDeltaNoConfArray(fromStateId: Int, withEvent: GenericEvent): List[(Int, TransitionOutput)] =
    super.getDeltaNoConfArray(fromStateId, withEvent)

}
