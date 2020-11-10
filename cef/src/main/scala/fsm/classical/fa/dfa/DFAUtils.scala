package fsm.classical.fa.dfa

import fsm.classical.fa.nfa.NFA
import scala.collection.mutable

class DFAUtils {

}

object DFAUtils {

  /**
    * Checks whether a DFA is m-unambiguous i.e., for every state there is only one sequence of symbols of length m that
    * can lead to it.
    *
    * @param dfa The DFA to check.
    * @param m The order.
    * @return True if the DFA is m-unambiguous.
    */
  def isMUnambiguous(
                      dfa: DFA,
                      m: Int
                    ): Boolean = {
    val dis = new Disambiguator(dfa, m)
    dis.isMUnambiguous(dfa, m)
  }

  /**
    * Converts a NFA to a DFA. Does not perform determinization. The NFA must already be DFA-equivalent. Acts more like
    * a recasting of the NFA as a DFA.
    *
    * @param nfa The original NFA.
    * @return The DFA.
    */
  def convertNfa2Dfa(nfa: NFA): DFA = {
    require(nfa.isDFAEquivalent, "NFA not equivalent to DFA")
    val inputSymbols = nfa.getInputSymbols
    val dfa = new DFA
    val nfaStates = nfa.getAllStates
    for ((nfaStateId, nfaState) <- nfaStates) {
      val dfaState = new DFAState(nfaStateId)
      for (symbol <- inputSymbols) {
        dfaState.addDelta(symbol, nfaState.getDelta(symbol).head)
      }
      if (nfa.isAccepting(nfaStateId)) dfaState.setOutput(mutable.Set("output"))
      if (nfa.isStart(nfaStateId)) dfaState.setAsStart(true)
      dfa.addState(nfaStateId, dfaState)
    }
    dfa.setInputSymbols(inputSymbols)
    dfa
  }

  /**
    * Disambiguates a given DFA up to a given order.
    *
    * @param dfa The given DFA.
    * @param order The given order
    * @return The disambiguated DFA.
    */
  def disambiguate(
                    dfa: DFA,
                    order: Int
                  ): DFA = {
    val dis = new Disambiguator(dfa, order)
    val dfaDis = dis.disambiguate()
    dfaDis
  }
}
