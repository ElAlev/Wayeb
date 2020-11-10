package fsm.classical.fa.nfa

import fsm.classical.pattern.regexp.RegExpTree
import scala.collection.mutable

class NFAFactory {

}

object NFAFactory {

  /**
    * Creates a streaming NFA from a streaming regular expression and adds any other symbols belonging to the alphabet,
    * but not present in the expression.
    *
    * @param regExpTree The regular expression, given as a tree. Must be a streaming expression, otherwise the
    *                   NFA semantics will be unclear.
    * @param stringSymbols The alphabet symbols.
    * @return The streaming NFA.
    */
  def buildEliminatedNFAForStream(
                                   regExpTree: RegExpTree,
                                   stringSymbols: scala.collection.immutable.Set[String]
                                 ): NFA = {
    val elnfa = buildEliminatedNFA(regExpTree)
    addExtraSymbols2NFAForStream(elnfa, stringSymbols)
  }

  /**
    * Constructs a NFA from a regular expression. The NFA will have no epsilon transitions.
    *
    * @param regExpTree The regular expression, given as a tree.
    * @return The NFA, without epsilon transitions.
    */
  def buildEliminatedNFA(regExpTree: RegExpTree): NFA = {
    val epsilonNFA = buildNFA(regExpTree)
    val elim = new Eliminator(epsilonNFA)
    elim.eliminate()
  }

  /**
    * Constructs a NFA from a regular expression. The NFA will have no epsilon transitions. Also adds extra symbols
    * that might not be present in the expression.
    *
    * @param regExpTree The regular expression, given as a tree.
    * @param stringSymbols The extra symbols to be added.
    * @return The NFA that can handle the extra symbols, without epsilon transitions.
    */
  def buildEliminatedNFA(
                          regExpTree: RegExpTree,
                          stringSymbols: scala.collection.immutable.Set[String]
                        ): NFA = {
    val elnfa = buildEliminatedNFA(regExpTree)
    addExtraSymbols2NFA(elnfa, stringSymbols)
  }

  /**
    * Constructs a NFA from a regular expression.
    *
    * @param regExpTree The regular expression, given as a tree.
    * @return The NFA that is equivalent to the given regular expression.
    */
  def buildNFA(regExpTree: RegExpTree): NFA = {
    val nfaTuple = NFAUtils.buildNFA(regExpTree, -1)
    val nfa = nfaTuple._1
    nfa
  }

  /**
    *  Constructs a NFA from a regular expression and adds to it extra symbols that might not be present in the regular
    *  expression.
    *
    * @param regExpTree The regular expression, given as a tree.
    * @param stringSymbols The extra symbols to be added.
    * @return The NFA that is equivalent to the given regular expression and can handle the extra symbols.
    */
  def buildNFA(
                regExpTree: RegExpTree,
                stringSymbols: scala.collection.immutable.Set[String]
              ): NFA = {
    val nfa = buildNFA(regExpTree)
    addExtraSymbols2NFA(nfa, stringSymbols)
  }

  /**
    * Adds extra symbols to a NFA that might not be present in it. We simply add to every state new transitions with
    * the new symbols taking us to a dead state.
    *
    * @param nfa The original NFA.
    * @param stringSymbols The extra symbols to be added. Nothing happens for those symbols that already exist in the
    *                      NFA.
    * @return The NFA that can handle the extra symbols.
    */
  private def addExtraSymbols2NFA(
                                   nfa: NFA,
                                   stringSymbols: scala.collection.immutable.Set[String]
                                 ): NFA = {
    val nfaSymbols = nfa.getInputSymbols
    val commonSymbols = nfaSymbols.intersect(stringSymbols)
    val onlyStringSymbols = mutable.Set.empty[String]
    for (ss <- stringSymbols) {
      if (!commonSymbols.contains(ss)) {
        onlyStringSymbols.add(ss)
      }
    }
    var deadStateId = -1
    // if there are indeed any new symbols, create a dead state, if it does not exist
    if (onlyStringSymbols.nonEmpty) {
      deadStateId = nfa.getDeadState
      if (deadStateId == -1) {
        val deadId = nfa.getAllStates.size
        val dead = new NFAState(deadId)
        nfa.addState(deadId, dead)
        nfa.setStateAsDead(deadId)
        deadStateId = deadId
      }
    }

    // then add transitions to the dead state from every other state with every new symbol
    for (ss <- onlyStringSymbols) {
      for ((sid, state) <- nfa.getAllStates) {
        state.addDelta(ss, deadStateId)
      }
    }

    val allSymbols = nfaSymbols.union(stringSymbols)
    nfa.setInputSymbols(allSymbols)

    if (deadStateId != -1) {
      for (as <- allSymbols) {
        nfa.addDelta(deadStateId, as, deadStateId)
      }
    }
    nfa
  }

  /**
    * Given a streaming NFA, we add extra symbols that might not be present in the NFA. The NFA is assumed to be already
    * streaming with its own symbols.
    *
    * @param nfa The original NFA.
    * @param stringSymbols Any extra symbols that might need to be added to the NFA.
    * @return The streaming NFA that can handle all given symbols.
    */
  private def addExtraSymbols2NFAForStream(
                                            nfa: NFA,
                                            stringSymbols: scala.collection.immutable.Set[String]
                                          ): NFA = {
    val nfaSymbols = nfa.getInputSymbols
    val commonSymbols = nfaSymbols.intersect(stringSymbols)
    // we first find the symbols that are not already handled by the NFA
    val onlyStringSymbols = mutable.Set.empty[String]
    for (ss <- stringSymbols) {
      if (!commonSymbols.contains(ss)) {
        onlyStringSymbols.add(ss)
      }
    }
    val startId = nfa.getStartId
    // now we add self-loop transitions on the start state with the new symbols
    for (ss <- onlyStringSymbols) {
      for ((sid, state) <- nfa.getAllStates) {
        state.addDelta(ss, startId)
      }
    }
    val allSymbols = nfaSymbols.union(stringSymbols)
    nfa.setInputSymbols(allSymbols)
    nfa
  }

  /*
  def buildNFAForStream(
      n: RegExpTree,
      stringSymbols: scala.collection.immutable.Set[String]): NFA = {
    val nfa = buildNFA(n)
    addExtraSymbols2NFAForStream(nfa, stringSymbols)
  }
   */

}
