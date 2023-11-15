package model.vmm.pst.spsa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.sdfa.SDFA
import model.vmm.mapper.Isomorphism
import model.vmm.pst.psa.ProbSuffixAutomaton
import model.vmm.{Symbol, SymbolWord}

object SPSAUtils extends LazyLogging {

  /**
    * Embeds a PSA in a SDFA.
    *
    * @param sdfa The SDFA MR of the original pattern R.
    * @param psa The PSA MS learned with the minterms of the SDFA.
    * @param iso The isomorphism mapping minterms to symbols.
    * @return The embedding M of the PSA in the SDFA.
    */
  def buildSPSA(
                 sdfa: SDFA,
                 psa: ProbSuffixAutomaton,
                 iso: Isomorphism
               ): SymbolicPSA = {
    val psaLabels = psa.getLabels
    val symbols: Set[Symbol] = psa.getSymbols
    // First create the initial states of the merged automaton by combining the initial
    // state of the SDFA with all the states of the PSA.
    val startId = sdfa.start
    val isStartFinal = sdfa.isFinal(startId)
    val startLabels = psaLabels.map(x => (startId, x))
    val startStates = startLabels.map(x => SPSAState(x._1, x._2, true, isStartFinal))
    // A frontier of states is created, including states of M that have no outgoing transitions yet.
    // First frontier consists of the initial states of M.
    var front: Set[SPSAState] = startStates
    var checked: Map[(Int, SymbolWord), SPSAState] = Map.empty
    while (front.nonEmpty) {
      val state2check = front.head // pick an element from Frontier
      for (symbol <- symbols) {
        val nextPSAState = psa.nextState(state2check.psaLabel, symbol)
        val nextPSAStateLabel = nextPSAState._1.label
        val nextPSAStateProb = nextPSAState._2
        val nextFSMState = sdfa.getDeltaWithSentence(state2check.sfaId, iso.getMinTermForSymbol(symbol))
        if (!checked.contains((nextFSMState, nextPSAStateLabel))) {
          // if we have not added this state, create it
          val newSPSAState = SPSAState(nextFSMState, nextPSAStateLabel, sdfa.isStart(nextFSMState), sdfa.isFinal(nextFSMState))
          state2check.addNextState(newSPSAState, symbol, nextPSAStateProb)
          front = front + newSPSAState
        } else {
          // else just add the transition
          val stateReached = checked((nextFSMState, nextPSAStateLabel))
          state2check.addNextState(stateReached, symbol, nextPSAStateProb)
        }
      }
      checked = checked + ((state2check.sfaId, state2check.psaLabel) -> state2check)
      front = front - state2check
    }
    SymbolicPSA(checked, iso)
  }

}
