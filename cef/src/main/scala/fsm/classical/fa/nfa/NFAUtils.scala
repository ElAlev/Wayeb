package fsm.classical.fa.nfa

import fsm.CountPolicy._
import fsm.classical.fa.nfa.NFAFactory.addExtraSymbols2NFA
import fsm.classical.pattern.regexp.{OperatorNode, RegExpTree, SymbolNode}
import fsm.classical.pattern.regexp.OperatorType._
import ui.ConfigUtils

class NFAUtils {

}

object NFAUtils {

  /**
    * Constructs a NFA from a regular expression, following Hopcroft et al., Introduction to automata, Section 3.2.3.
    *
    * @param regExpTree The regular expression, given as a tree.
    * @param lastStateId the id from which we can start assigning new ids. The function is called recursively and each
    *                    sub-call returns a sub-NFA with its own state ids. Each time we need to know which ids have
    *                    already been assigned to avoid re-assigning them.
    * @return The NFA constructed and the id of the last state created (max id).
    */
  def buildNFA(
                regExpTree: RegExpTree,
                lastStateId: Int
              ): (NFA, Int) = {
    var stateId = lastStateId
    var nfa: NFA = null
    regExpTree match {
      case SymbolNode(symbol, _) => {
        // If the expression is a single symbol, create a start and a final state and connect them.
        nfa = new NFA
        stateId += 1
        val startState = new NFAState(stateId)
        //startState.setAsStart(true)
        startState.addDelta(symbol, stateId + 1)
        nfa.addState(stateId, startState)
        nfa.setStateAsStart(stateId)
        stateId += 1
        val acceptState = new NFAState(stateId)
        //acceptState.setAsAccepting(true)
        nfa.addState(stateId, acceptState)
        nfa.setStateAsAccepting(stateId)
        nfa.setInputSymbols(scala.collection.mutable.Set(symbol))
      }
      case OperatorNode(operator, ch) => {
        // If the expression contains an operator, first create the NFA for the first (left) operand
        val leftChild = ch.head
        val leftNfaId = buildNFA(leftChild, lastStateId)
        val leftNfa = leftNfaId._1
        val leftId = leftNfaId._2
        if (operator == CONCAT) {
          // If the operator is concatenation, also create the NFA for the second (right) operand
          val rightChild = ch(1)
          val rightNfaId = buildNFA(rightChild, leftId)
          val rightNfa = rightNfaId._1
          val rightId = rightNfaId._2
          // and then connect the two sub-NFAs
          nfa = concatNFAs(leftNfa, rightNfa)
          stateId = rightId
        } else if (operator == UNION) {
          // If the operator is union, also create the NFA for the second (right) operand
          val rightChild = ch(1)
          val rightNfaId = buildNFA(rightChild, leftId)
          val rightNfa = rightNfaId._1
          val rightId = rightNfaId._2
          // and then connect the two sub-NFAs
          nfa = unionizeNFAs(leftNfa, rightNfa)
          stateId = rightId + 2
        } else {
          // If the operator is iteration, then apply iteration on the sub-NFA
          nfa = iterateNFA(leftNfa)
          stateId = leftId + 2
        }
      }
    }

    (nfa, stateId)
  }

  /**
    * Connects two NFAs with concatenation.
    *
    * @param leftNfa The left NFA to be connected.
    * @param rightNfa The right NFA to be connected.
    * @return The connected NFA.
    */
  private def concatNFAs(
                          leftNfa: NFA,
                          rightNfa: NFA
                        ): NFA = {
    val nfa = new NFA
    nfa.addStates(leftNfa.getAllStates)
    nfa.addStates(rightNfa.getAllStates)
    // the start state of the left NFA should be the new start state
    nfa.setStateAsStart(leftNfa.getStartId)
    // the final states of the right NFA should be the new final states
    nfa.setStatesAsAccepting(rightNfa.getAcceptingId)
    // we connect the final state of the left NFA to the start state of the right NFA with an epsilon transition
    leftNfa.getAcceptingStates.head.addDelta(ConfigUtils.epsilonSymbol, rightNfa.getStartId)
    nfa.setInputSymbols(leftNfa.getInputSymbols ++ rightNfa.getInputSymbols)
    nfa.addInputSymbol(ConfigUtils.epsilonSymbol)
    nfa
  }

  /**
    * Connects two NFAs with union.
    *
    * @param leftNfa The left NFA to be connected.
    * @param rightNfa The right NFA to be connected.
    * @return The connected NFA.
    */
  private def unionizeNFAs(
                            leftNfa: NFA,
                            rightNfa: NFA
                          ): NFA = {
    val nfa = new NFA
    nfa.addStates(leftNfa.getAllStates)
    nfa.addStates(rightNfa.getAllStates)
    // we create a new start state
    val newStartId = rightNfa.getAcceptingId.head + 1
    val newAcceptingId = newStartId + 1
    val newStartState = new NFAState(newStartId)
    // we connect this new start state to the start states of the sub-NFAs with an epsilon transition
    newStartState.addDelta(ConfigUtils.epsilonSymbol, leftNfa.getStartId)
    newStartState.addDelta(ConfigUtils.epsilonSymbol, rightNfa.getStartId)
    nfa.addState(newStartId, newStartState)
    nfa.setStateAsStart(newStartId)
    // we create a new final state
    val newAcceptingState = new NFAState(newAcceptingId)
    // we connect the final states of the sub-NFAs to the new final state with an epsilon transition
    leftNfa.getAcceptingStates.head.addDelta(ConfigUtils.epsilonSymbol, newAcceptingId)
    rightNfa.getAcceptingStates.head.addDelta(ConfigUtils.epsilonSymbol, newAcceptingId)
    nfa.addState(newAcceptingId, newAcceptingState)
    nfa.setStateAsAccepting(newAcceptingId)
    nfa.setInputSymbols(leftNfa.getInputSymbols ++ rightNfa.getInputSymbols)
    nfa.addInputSymbol(ConfigUtils.epsilonSymbol)
    nfa
  }

  /**
    * Applies iteration on a sub-NFA. See Hopcroft et al., Introduction to automata for details.
    *
    * @param leftNfa The NFA on which to apply iteration.
    * @return The iterated NFA.
    */
  private def iterateNFA(leftNfa: NFA): NFA = {
    val nfa = new NFA
    nfa.addStates(leftNfa.getAllStates)
    leftNfa.getAcceptingStates.head.addDelta(ConfigUtils.epsilonSymbol, leftNfa.getStartId)
    val newStartId = leftNfa.getAcceptingId.head + 1
    val newAcceptingId = newStartId + 1
    val newAcceptingState = new NFAState(newAcceptingId)
    nfa.addState(newAcceptingId, newAcceptingState)
    nfa.setStateAsAccepting(newAcceptingId)
    leftNfa.getAcceptingStates.head.addDelta(ConfigUtils.epsilonSymbol, newAcceptingId)
    val newStartState = new NFAState(newStartId)
    newStartState.addDelta(ConfigUtils.epsilonSymbol, newAcceptingId)
    newStartState.addDelta(ConfigUtils.epsilonSymbol, leftNfa.getStartId)
    nfa.addState(newStartId, newStartState)
    nfa.setStateAsStart(newStartId)
    nfa.setInputSymbols(leftNfa.getInputSymbols)
    nfa.addInputSymbol(ConfigUtils.epsilonSymbol)
    nfa
  }

  /**
    * Applies a counting policy to a NFA. The OVERLAP policy allows matches to be overlapping. The NONOVERLAP policy
    * requires that matches are not overlapping. Using the default construction algorithm
    * (see fsm.classical.nfa.NFAUtils#buildNFA(pattern.regexp.RegExpTree, int)), the resulting NFA's policy is by
    * default OVERLAP. If we want the NONOVERLAP policy, we need to modify the NFA so that after every match, it has to
    * be "restarted". We achieve this by deleting all outgoing transition from the final states and copying onto them
    * the outgoing transitions of the start state. By doing this, the final states are made equivalent to the start
    * state. Whenever the NFA reaches a final state, from that point on, it is as if it has reached the start state.
    *
    * @param initialNfa The initial NFA.
    * @param policy The counting policy. OVERLAP or NONOVERLAP
    * @return If OVERLAP, the initial NFA is returned. If NONOVERLAP, the NFA with its final states made equivalent to
    *         the start state.
    */
  def setCountPolicy(
                      initialNfa: NFA,
                      policy: CountPolicy
                    ): NFA = {
    if (policy == NONOVERLAP) {
      val start = initialNfa.getStartState
      val states = initialNfa.getAllStates
      val inputSymbols = initialNfa.getInputSymbols
      for ((id, state) <- states) {
        if (initialNfa.isAccepting(id)) {
          // delete all outgoing transitions from finals
          state.clearDelta()
          for (is <- inputSymbols) {
            // copy outgoing transitions of start state as transitions of the final state
            val nexts = start.getDelta(is)
            if (nexts.nonEmpty) {
              for (next <- nexts) {
                state.addDelta(is, next)
              }
            }
          }
        }
      }
      // Some states may become detached after resetting, so we have to remove them.
      // e.g.: (a+b) + (b.b), i.e. it recognizes a,b,bb and b is prefix of bb
      var det = initialNfa.detached()
      while (det.nonEmpty) {
        for ((id, _) <- states) {
          if (id != initialNfa.getStartId & initialNfa.isDetached(id)) initialNfa.removeState(id)
        }
        det = initialNfa.detached()
      }
    }
    initialNfa
  }

  /**
    * Converts a LearnLib DFA to a (non-streaming) NFA.
    *
    * @param states The state ids of the LLDFA.
    * @param transitions The LLDFA transitions given as a set of (source: Int,target: Int,symbol: String) tuples.
    * @param start The id of the start state.
    * @param finals The ids of the final states.
    * @return A NFA corresponding to the provided LLDFA.
    */
  def lldfa2nfaNoStream(
                         states: Set[Int],
                         transitions: Set[(Int, Int, String)],
                         start: Int,
                         finals: Set[Int]
                       ): NFA = {
    val nfa = new NFA
    val symbols = transitions.map(t => t._3)
    // we add all symbols
    symbols.foreach(s => nfa.addInputSymbol(s))
    // then all states
    states.foreach(state => nfa.addState(state, new NFAState(state)))
    // We also need to add a dead state, because there might exist LLDFA states that have no transition for some
    // symbols. For these symbols, we will create transitions to the dead state.
    val deadStateId = nfa.getAllStates.keySet.max + 1
    nfa.addState(deadStateId, new NFAState(deadStateId))
    // we add all transitions
    transitions.foreach(t => nfa.addDelta(t._1, t._3, t._2))

    // For every state, if it has no outgoing transitions with a symbol, make it go to a dead state for this symbol.
    states.foreach(stateId => {
      symbols.foreach(symbol => {
        val state = nfa.getState(stateId)
        if (!state.hasSymbol(symbol)) nfa.addDelta(stateId, symbol, deadStateId)
      })
    })
    symbols.foreach(symbol => nfa.addDelta(deadStateId, symbol, deadStateId))

    finals.foreach(f => nfa.setStateAsAccepting(f))
    nfa.setStateAsStart(start)
    nfa
  }

  /**
   * From a given NFA, eliminates all epsilon transitions. Also adds transitions for all symbols.
   *
   * @param nfa The original NFA, possibly with epsilon transitions.
   * @param stringSymbols All symbols that the NFA must handle.
   * @return A NFA without epsilon transitions that can handle all symbols.
   */
  def eliminateEpsilon(
                        nfa: NFA,
                        stringSymbols: scala.collection.immutable.Set[String]
                      ): NFA = {
    val eliminator = new Eliminator(nfa)
    val eliminated = eliminator.eliminate()
    NFAFactory
    addExtraSymbols2NFA(eliminated, stringSymbols)
  }

  /**
    * Converts a LearnLib DFA to a streaming NFA.
    *
    * @param states The state ids of the LLDFA.
    * @param transitions The LLDFA transitions given as a set of (source: Int,target: Int,symbol: String) tuples.
    * @param start The id of the start state.
    * @param finals The ids of the final states.
    * @return A streaming NFA corresponding to the provided LLDFA.
    */
  def lldfa2nfa(
                 states: Set[Int],
                 transitions: Set[(Int, Int, String)],
                 start: Int,
                 finals: Set[Int]
               ): NFA = {
    val nfaStream = new NFA
    val symbols = transitions.map(t => t._3)
    // we add all symbols
    symbols.foreach(s => nfaStream.addInputSymbol(s))
    // then all states
    states.foreach(state => nfaStream.addState(state, new NFAState(state)))
    // and all transitions
    transitions.foreach(t => nfaStream.addDelta(t._1, t._3, t._2))

    finals.foreach(f => nfaStream.setStateAsAccepting(f))

    // we now need to make the NFA streaming, so we create a new start state and connect it to the old start state with
    // an epsilon transition
    val startStateId = nfaStream.getAllStates.keySet.max + 1
    nfaStream.addState(startStateId, new NFAState(startStateId))
    nfaStream.addInputSymbol(ConfigUtils.epsilonSymbol)
    nfaStream.addDelta(startStateId, ConfigUtils.epsilonSymbol, start)
    // we also need to add self-loop transitions on the new start state with all the symbols
    symbols.foreach(s => nfaStream.addDelta(startStateId, s, startStateId))

    nfaStream.setStateAsStart(startStateId)

    // we finally eliminate all epsilon transitions.
    val eliminator = new Eliminator(nfaStream)
    val nfaElim = eliminator.eliminate()
    //val nfaCount = setCountPolicy(nfaElim, ConfigUtils.defaultPolicy)
    //nfaCount
    nfaElim

  }

}
