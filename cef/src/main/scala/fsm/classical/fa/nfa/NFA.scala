package fsm.classical.fa.nfa

import ui.ConfigUtils

import scala.collection.{SortedMap, mutable}

/**
  * Class representing classical non-deterministic finite automata (NFA).
  * NFA are constructed incrementally, by calling fsm.classical.nfa.NFA#addState(int, fsm.classical.nfa.NFAState)
  * and fsm.classical.nfa.NFA#addDelta(int, java.lang.String, int).
  */
class NFA private[nfa] {
  private val states: mutable.Map[Int, NFAState] = mutable.Map.empty[Int, NFAState]
  private var inputSymbols: mutable.Set[String] = mutable.Set.empty[String]
  private var start = -1
  private val accepting: mutable.Set[Int] = mutable.Set.empty[Int]
  private var dead = -1

  /**
    * Adds a new transition to the NFA.
    *
    * @param source The id of the source state (must already exist as a state of the NFA).
    * @param symbol The transition's symbol (must already exist as a symbol of the NFA).
    * @param target The id of the target state (must already exist as a state of the NFA).
    */
  def addDelta(
                source: Int,
                symbol: String,
                target: Int
              ): Unit = {
    require(states.contains(source) & states.contains(target) & inputSymbols.contains(symbol))
    states(source).addDelta(symbol, target)
  }

  /**
    * Adds a new state to the NFA. If a state with the given id already exists, it will be replaced.
    *
    * @param stateId The id of the new state.
    * @param state The new state.
    */
  def addState(
                stateId: Int,
                state: NFAState
              ): Unit = states += (stateId -> state)

  /**
    * Finds the enclosure of a state. For a definition of enclosure, see
    * @book{DBLP:books/daglib/0016921,
    *       author    = {John E. Hopcroft and
    *       Rajeev Motwani and
    *       Jeffrey D. Ullman},
    *       title     = {Introduction to automata theory, languages, and computation, 3rd Edition},
    *       series    = {Pearson international edition},
    *       publisher = {Addison-Wesley},
    *       year      = {2007}
    *       }
    *
    * @param stateId The id of the state whose enclosure we want to find.
    * @return The enclosure, as a set of state ids.
    */
  def enclose(stateId: Int): mutable.Set[Int] = {
    require(states.contains(stateId))
    enclose2(stateId, mutable.Set[Int]())
  }

  /**
    * Helper, recursive function to find the enclosure of a state.
    * First finds the epsilon targets of the given state, then the epsilon targets of those states, etc.
    *
    * @param stateId The id of the next state to check (in first call, it should be the id of the state we want to check).
    * @param checked The set of ids of states that have already been checked (should be empty in first call).
    * @return The enclosure of the state, given that all other states in checked have already been explored.
    */
  private def enclose2(
                        stateId: Int,
                        checked: mutable.Set[Int]
                      ): mutable.Set[Int] = {
    var enclosure = mutable.Set[Int](stateId)
    val deltaEpsilon = states(stateId).getDelta(ConfigUtils.epsilonSymbol)
    if (deltaEpsilon.nonEmpty) {
      for (se <- deltaEpsilon) {
        if (!checked.contains(se)) {
          val thisChecked = checked + stateId
          enclosure = enclosure ++ enclose2(se, thisChecked)
        }
      }
    }
    enclosure
  }

  /**
    * Finds the enclosure of a set of states.
    *
    * @param stateIdSet The ids of the states whose enclosure we want to find.
    * @return The enclosure of the given states, given as a single set.
    */
  def enclose(stateIdSet: scala.collection.immutable.Set[Int]): mutable.Set[Int] = {
    var enclosure = mutable.Set[Int]()
    for (s <- stateIdSet) {
      enclosure = enclosure ++ enclose(s)
    }
    enclosure
  }

  /**
    * Retrieves the states we can reach from a given source state with a given symbol.
    * The source state must already exist.
    * If symbol does not exist or there are no transitions with this symbol from the given state,
    * an empty set is returned.
    *
    * @param source The id of the source state.
    * @param symbol The given symbol.
    * @return The states reached, given as a set of ids.
    */
  def getDelta(
                source: Int,
                symbol: String
              ): mutable.Set[Int] = {
    require(states.contains(source))
    states(source).getDelta(symbol)
  }

  /**
    * Retrieves the states we can reach from a given set of source states with a given symbol.
    *
    * @param sourceStates The ids of the source states.
    * @param symbol The given symbol.
    * @return The states reached, given as a set of ids.
    */
  def getDelta(
                sourceStates: mutable.Set[Int],
                symbol: String
              ): mutable.Set[Int] = {
    var res = mutable.Set[Int]()
    for (s <- sourceStates) {
      res = res ++ getDelta(s, symbol)
    }
    res
  }

  /**
    * Retrieves the states we can reach from a given source state with a given symbol.
    * Same as fsm.classical.nfa.NFA#getDelta(int, java.lang.String), but also takes into account epsilon transitions.
    * We also follow any epsilon transitions to find the reached states.
    *
    * @param source The id of the source state.
    * @param symbol The given symbol.
    * @return The states reached, given as a set of ids.
    */
  def getDeltaWithEpsilon(
                           source: Int,
                           symbol: String
                         ): mutable.Set[Int] = {
    require(states.contains(source))
    var res = mutable.Set[Int]()
    val statesEpsilon = enclose(source)
    var statesChar = mutable.Set[Int]()
    for (se <- statesEpsilon) {
      statesChar = statesChar ++ getDelta(se, symbol)
    }
    for (sc <- statesChar) {
      res = res ++ enclose(sc)
    }
    res
  }

  /**
    * Determines whether the given word belongs to the NFA's language.
    *
    * @param word The given word, given as a list. The head of the list is the first element of the word.
    * @return True if the word is accepted by the NFA.
    */
  def accepts(word: List[String]): Boolean = accepts(start, word)

  /**
    * Determines whether the NFA, starting from a given state, can reach a final state.
    *
    * @param stateId The id of the given state.
    * @param word The given word, given as a list. The head of the list is the first element of the word.
    * @return True if we can reach a final state from the given state with the given word.
    */
  def accepts(
               stateId: Int,
               word: List[String]
             ): Boolean = {
    require(word.nonEmpty)
    val currentSymbol = word.head
    val nextStates = getDeltaWithEpsilon(stateId, currentSymbol)
    if (nextStates.isEmpty) return false
    if (word.size == 1) {
      if (nextStates.intersect(accepting).nonEmpty) return true
      else return false
    } else {
      for (ns <- nextStates) {
        if (accepts(ns, word.tail)) return true
      }
      return false
    }
    false
  }

  /**
    * Removes a state from the NFA. CAUTION: does not remove any relevant transitions.
    *
    * @param stateId The id of the state to be removed.
    */
  def removeState(stateId: Int): Unit = states.remove(stateId)

  /**
    * A map of (id,state) with new states to add.
    *
    * @param newStates The new states to be added. Throws error if one of the new states already exists.
    */
  def addStates(newStates: mutable.Map[Int, NFAState]): Unit = {
    for ((k, v) <- newStates) {
      if (states.contains(k)) throw new IllegalArgumentException("There is already a state with id " + k + " in NFA")
      states += (k -> v)
    }
  }

  /**
    * Adds a new symbol.
    *
    * @param newSymbol The new symbol to be added.
    */
  def addInputSymbol(newSymbol: String): Unit = inputSymbols += newSymbol

  /**
    * Adds new symbols.
    *
    * @param newSymbols The set of new symbols to be added.
    */
  def addInputSymbols(newSymbols: mutable.Set[String]): Unit = inputSymbols = inputSymbols ++ newSymbols

  /**
    * Sets the set of symbols (overwrites previous set).
    *
    * @param inputSymbolsToSet The symbols.
    */
  def setInputSymbols(inputSymbolsToSet: mutable.Set[String]): Unit = inputSymbols = inputSymbolsToSet

  /**
    * Sets a state as a start state. Overwrites previous.
    *
    * @param stateId The id of the state to be set as start. Must already exist as a NFA state.
    */
  def setStateAsStart(stateId: Int): Unit = {
    require(states.contains(stateId))
    start = stateId
  }

  /**
    * Sets a state as accepting/final. Adds it to any existing final states.
    *
    * @param stateId The id of the state to be set as final. Must already exist as an NFA state.
    */
  def setStateAsAccepting(stateId: Int): Unit = {
    require(states.contains(stateId))
    accepting.add(stateId)
  }

  /**
    * Sets states as finals. Adds them to any existing finals.
    *
    * @param stateIds The ids of the states to be set as final. Must already exist as NFA states.
    */
  def setStatesAsAccepting(stateIds: mutable.Set[Int]): Unit = stateIds.foreach(stateId => setStateAsAccepting(stateId))

  /**
    * Sets a state as dead. Overwrites previous.
    *
    * @param stateId The id of the state to be set as dead.
    */
  def setStateAsDead(stateId: Int): Unit = dead = stateId

  /**
    * @return The id of the dead state.
    */
  def getDeadState: Int = dead

  /**
    * @return The id of the start state.
    */
  def getStartId: Int = start

  /**
    * @return The ids of the final states.
    */
  def getAcceptingId: mutable.Set[Int] = accepting

  /**
    * Checks whether a given state is the start state.
    *
    * @param stateId The id if the state to check. Must already exist.
    * @return True if the state is the start state.
    */
  def isStart(stateId: Int): Boolean = {
    require(states.contains(stateId))
    start == stateId
  }

  /**
    * Checks whether a given state is final.
    *
    * @param stateId The id of the state to check. Must already exist.
    * @return True if the given state is final.
    */
  def isAccepting(stateId: Int): Boolean = {
    require(states.contains(stateId))
    accepting.contains(stateId)
  }

  /**
    * Determines whether a given state is detached, i.e., there does not exist any incoming transition to it from
    * another state.
    *
    * @param stateId The id of the state to check.
    * @return True if the state is detached.
    */
  def isDetached(stateId: Int): Boolean = {
    if (isStart(stateId)) false
    else !states.exists(x => x._2.getAllNext.contains(stateId))
  }

  /**
    * @return All detached states.
    */
  def detached(): scala.collection.immutable.Set[Int] = states.keys.filter(isDetached).toSet

  /**
    * @return The start state.
    */
  def getStartState: NFAState = states(start)

  /**
    * @return All final states.
    */
  def getAcceptingStates: mutable.Set[NFAState] = {
    val acceptingStates = mutable.Set[NFAState]()
    for (a <- accepting) {
      acceptingStates.add(states(a))
    }
    acceptingStates
  }

  /**
    * @return All states, with their ids.
    */
  def getAllStates: mutable.Map[Int, NFAState] = states

  /**
    * Retrieves the state with the given id.
    *
    * @param stateId The id of the state to retrieve. Must already exist.
    * @return The state.
    */
  def getState(stateId: Int): NFAState = {
    require(states.contains(stateId))
    states(stateId)
  }

  /**
    * @return All input symbols.
    */
  def getInputSymbols: mutable.Set[String] = inputSymbols

  /**
    * Determines whether the NFA is DFA-equivalent. A NFA is DFA-equivalent if every NFA is DFA-equivalent.
    * See fsm.classical.nfa.NFA#stateDFAequivalent(int).
    *
    * @return True if the NFA is DFA-equivalent.
    */
  def isDFAEquivalent: Boolean = {
    for (sid <- states.keySet) {
      if (!stateDFAequivalent(sid)) return false
    }
    true
  }

  /**
    * Determines whether a given state is DFA-equivalent. A state is DFA-equivalent if it has an outgoing transition
    * for every symbol and for every symbol there exists only one outgoing transition.
    *
    * @param stateId The id of the state to check.
    * @return True if the state is DFA-equivalent.
    */
  def stateDFAequivalent(stateId: Int): Boolean = {
    require(states.contains(stateId))
    val state = states(stateId)
    for (a <- inputSymbols) {
      if (!state.hasSymbol(a)) { println("state " + stateId.toString + " has no symbol " + a); return false }
      else if (state.getDelta(a).size != 1) return false
    }
    true
  }

  override def toString: String = {
    var v = mutable.Set[Int]()
    var d = ""
    var t = mutable.Set[Int]()
    val sorted = SortedMap(states.toSeq: _*)
    for ((key, value) <- sorted) {
      v += key
      d += "\nState: " + key + "\n" + value.toString
      if (accepting.contains(key)) t += key
    }
    //println("V=" + v +"\nA=" + inputSymbols + "\nf=" + d + "\nT=" + t)
    val s = "V=" + v + "\nq0=" + start + "\nqdead=" + dead + "\nT=" + t + "\nA=" + inputSymbols + "\nf=" + d
    s
  }

  /*
  def addShiftedStates(newStates: mutable.Map[Int, NFAState], shift: Int): Unit = {
    for ((k, v) <- newStates) {
      if (states.contains(k)) throw new IllegalArgumentException("There is already a state with id " + k + " in NFA")
      states += ((k + 1) -> v)
    }
  }
  */

}
