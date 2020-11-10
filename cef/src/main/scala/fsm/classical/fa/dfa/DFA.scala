package fsm.classical.fa.dfa

import java.io.{FileOutputStream, ObjectOutputStream}
import fsm.classical.FATransition
import fsm.classical.fa.nfa.{NFAFactory, NFAUtils}
import fsm.CountPolicy.CountPolicy
import fsm.classical.pattern.regexp.{RegExpReader, RegExpTree, RegExpUtils}
import scala.collection.{SortedMap, mutable}

object DFA {

  /**
    * Constructs a DFA from a pattern.
    *
    * @param pattern The pattern, given as a regular expression tree.
    * @param countPolicy The counting policy, OVERLAP or NONOVERLAP.
    * @param streamSymbols All the symbols that may be encountered in the stream.
    * @param order The disambiguation order.
    * @return The disambiguated, streaming DFA corresponding to the given pattern.
    */
  def apply(
             pattern: RegExpTree,
             countPolicy: CountPolicy,
             streamSymbols: scala.collection.immutable.Set[String],
             order: Int
           ): DFA = {
    require(RegExpUtils.leastSymbolsNo(pattern) != 0, "Degenerate expression")
    val elnfa = NFAFactory.buildEliminatedNFAForStream(pattern, streamSymbols)
    val nfa = NFAUtils.setCountPolicy(elnfa, countPolicy)
    var dfa = DFAUtils.convertNfa2Dfa(nfa) //TODO: .withMemory(order)
    if (order > 0) {
      val dis = new Disambiguator(dfa, order)
      dfa = dis.disambiguate()
    }
    dfa
  }

  /**
    * Constructs a DFA from a regular expression in a file.
    *
    * @param patternFile The path to the file containing the regular expression.
    * @param countPolicy The counting policy, OVERLAP or NONOVERLAP.
    * @param streamSymbols All the symbols that may be encountered in the stream.
    * @param order The disambiguation order.
    * @return The disambiguated, streaming DFA corresponding to the given pattern.
    */
  def apply(
             patternFile: String,
             countPolicy: CountPolicy,
             streamSymbols: scala.collection.immutable.Set[String],
             order: Int
           ): DFA = {
    val rer = new RegExpReader(patternFile, streamSymbols.map(x => x))
    val patternRE = rer.getRegExpTreeWithSigmaStar._1
    DFA(patternRE, countPolicy, streamSymbols, order)
  }
}

/**
  * Class representing classical deterministic finite automata (DFA).
  * DFA are constructed incrementally, by calling fsm.classical.dfa.DFA#addState(int, fsm.classical.dfa.DFAState).
  */
class DFA private[dfa] extends Serializable {
  private val states = mutable.Map.empty[Int, DFAState]
  private var inputSymbols = mutable.Set.empty[String]
  private var order = 0
  private var pattern = ""
  private var id = 0

  /**
    * Adds a new state.
    *
    * @param stateId The id of the state to be added.
    * @param state The state to be added.
    */
  def addState(
                stateId: Int,
                state: DFAState
              ): Unit = states += (stateId -> state)

  /**
    * @return A list with all transitions of the DFA.
    */
  def getTransitions: List[FATransition] = {
    val transitions = states.map(s => s._2.getTransitions).toList.flatten
    transitions
  }

  /**
    * Retrieves all states we can reach from the given state, regardless of the symbol.
    *
    * @param stateId The id of the given state.
    * @return A set with the ids of all reachable states.
    */
  private def getNextStates(stateId: Int): scala.collection.immutable.Set[Int] = {
    val transitions = getTransitions
    transitions.filter(t => t.source == stateId).map(t => t.target).toSet
  }

  /**
    * Checks whether two states are connected with a transition.
    *
    * @param from The candidate source state.
    * @param to The candidate target state.
    * @return True if from is connected to to.
    */
  def connected(
                 from: Int,
                 to: Int
               ): Boolean = {
    val next = getNextStates(from)
    next.contains(to)
  }

  /**
    * Sets the input symbols.
    *
    * @param inputSymbolsToSet The set of symbols to be set as input symbols.
    */
  def setInputSymbols(inputSymbolsToSet: mutable.Set[String]): Unit = inputSymbols = inputSymbolsToSet

  /**
    * Finds the state reached from a given state with a given symbol.
    *
    * @param source The id of the given state.
    * @param symbol The given symbol.
    * @return The id of the state reached. 0 indicates that no state can be reached with the given symbol.
    */
  def delta(
             source: Int,
             symbol: String
           ): Int = {
    var ns = 0
    ns = states(source).getDelta(symbol)
    if (ns == -1) ns = 0
    ns
  }

  /**
    * Finds the state reached from a given state with a given word.
    *
    * @param source The id of the given state.
    * @param word The given word, given as a list of symbols. The head of the list is the first element.
    * @return The id of the state reached. 0 indicates that no state can be reached with the given word.
    */
  def delta(
             source: Int,
             word: List[String]
           ): Int = {
    require(word.nonEmpty)
    var ns = source
    for (c <- word) ns = delta(ns, c)
    ns
  }

  /**
    * Finds the state reached from a given state with a given set of symbols.
    *
    * @param source The id of the given state.
    * @param symbols The set of symbols.
    * @return A set with the ids of all the states we can reach with the given symbols.
    */
  def delta(
             source: Int,
             symbols: mutable.Set[String]
           ): mutable.Set[Int] = {
    require(symbols.nonEmpty)
    var ns: mutable.Set[Int] = mutable.Set.empty
    for (c <- symbols) ns += delta(source, c)
    ns
  }

  /**
    * Determines whether a given word is accepted by the DFA.
    *
    * @param word The given word, as a list of symbols. The head is the first symbol.
    * @return True if the DFA accepts the word.
    */
  def accepts(word: List[String]): Boolean = {
    val start = getStart
    val finals = getAllFinals
    val endsAt = delta(start, word)
    finals.contains(endsAt)
  }

  /**
    * Checks whether the given state has a self-loop transition with the given symbol.
    *
    * @param stateId The id of the given state.
    * @param symbol The given symbol.
    * @return True if there does exist a self-loop transition with the given symbol.
    */
  def isLoopStateOn(
                     stateId: Int,
                     symbol: String
                   ): Boolean = delta(stateId, symbol) == stateId

  /**
    * @return the id of the start state (or -1 if no start state exists)
    */
  def getStart: Int = {
    val startStates = states.filter(s => s._2.isStart)
    if (startStates.nonEmpty) startStates.head._1 else -1
  }

  /**
    * @return The ids of all final states.
    */
  def getAllFinals: mutable.Set[Int] = {
    val f = mutable.Set[Int]()
    for ((k, v) <- states) {
      if (v.isFinal) {
        f += k
      }
    }
    f
  }

  /**
    * @return The ids of all non-final states.
    */
  def getAllNonFinals: mutable.Set[Int] = {
    val f = mutable.Set[Int]()
    for ((k, v) <- states) {
      if (!v.isFinal) {
        f += k
      }
    }
    f
  }

  /**
    * Retrieves the output of the given state.
    *
    * @param stateId The id of the given state.
    * @return The output as a set of strings.
    */
  def getOutput(stateId: Int): mutable.Set[String] = states(stateId).getOutput

  /**
    * @return all states with their ids.
    */
  def getStates: mutable.Map[Int, DFAState] = states

  /**
    * @return all state ids, sorted.
    */
  def getStateKeys: mutable.Set[Int] = {
    val sorted = SortedMap(states.toSeq: _*)
    var s: mutable.Set[Int] = mutable.Set[Int]()
    for ((k, v) <- sorted) {
      s += k
    }
    s
  }

  /**
    * @return all input symbols.
    */
  def getInputSymbols: mutable.Set[String] = inputSymbols

  /**
    * @return the number of states.
    */
  def size: Int = states.size

  /**
    * @return the maximum state id.
    */
  def getMaxId: Int = states.keySet.max

  /**
    * Sets the order and the state labels of the DFA. Each state is supposed to have a set of "labels", i.e., a set of
    * symbol sequences of length o which can lead to the state. We can find ourselves in this state by following one of
    * these sequences.
    *
    * @param o The given order.
    * @param dq The labels, as a map of ids to sets of words of length o. The set of ids of the labels must be the same
    *           as the set of DFA ids.
    */
  def setOrderLabel(
                     o: Int,
                     dq: mutable.Map[Int, mutable.Set[List[String]]]
                   ): Unit = {
    require(o >= 0)
    require(dq.keySet == states.keySet)
    order = o
    for ((k, v) <- states) {
      v.setOrder(o)
      v.setLabel(dq(k))
    }
  }

  /**
    * Finds all the duplicates of a given state.
    *
    * @param stateId The id of the given state.
    * @return A list with the ids of all duplicates.
    */
  def getDuplicatesOf(stateId: Int): List[Int] = {
    var dups = List[Int]()
    for ((k, v) <- states) {
      if (v.isDuplicateOf(stateId)) dups = dups ::: List(k)
    }
    dups
  }

  /**
    * Finds all the deep duplicates of a given state, i.e., its duplicate states and the duplicates of the first
    * duplicate states etc.
    *
    * @param stateId The id of the given state.
    * @return A list with the ids of all deep duplicates.
    */
  def getDeepDuplicatesOf(stateId: Int): List[Int] = {
    var dups = List[Int]()
    for ((k, v) <- states) {
      if (v.isDuplicateOf(stateId)) {
        dups = dups ::: List(k)
        dups = dups ::: getDeepDuplicatesOf(k)
      }
    }
    dups
  }

  /**
    * @return All states and their duplicates. We order all non-duplicate states and append their duplicates.
    */
  def getStatesOrdered: List[Int] = {
    var dups = List[Int]()
    val sorted = SortedMap(states.toSeq: _*)
    for ((k, v) <- sorted) {
      if (!v.isDuplicate) {
        dups = dups ::: List(k)
        dups = dups ::: getDeepDuplicatesOf(k)
      }
    }
    dups
  }

  /**
    * @return A string representing the DFA. Might be too slow in debugging if the DFA is large.
    */
  override def toString: String = printDFA

  /**
    * @return A string representing the DFA.
    */
  def printDFA: String = {
    var v = mutable.Set[Int]()
    var d = ""
    var t = mutable.Set[Int]()
    val sorted = SortedMap(states.toSeq: _*)
    for ((key, value) <- sorted) {
      v += key
      d += "\nState: " + key + " (label: " + value.getLabel + ", Duplicate of: " + value.getOriginal + " Duplicates: " + getDeepDuplicatesOf(key) + ")" + "\n" + value.printDelta
      if (value.isFinal) t += key
    }
    //println("V=" + v +"\nA=" + inputSymbols + "\nf=" + d + "\nT=" + t)
    val s = "V=" + v + "\nVordered=" + getStatesOrdered + "\nA=" + inputSymbols + "\nf=" + d + "\nT=" + t + "\norder=" + order
    s
  }

  /**
    * Serializes DFA and writes it to a file.
    *
    * @param fn The path to the file.
    */
  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

  /*

  def getId: Int = id

  private[dfa] def setPattern(p: String): Unit = pattern = p

  private[dfa] def setId(i: Int): Unit = id = i

  def append(dfa: DFA): Unit = {
    inputSymbols = inputSymbols ++ dfa.getInputSymbols
    val s = states.size
    if (s == 0) {
      for ((k, v) <- dfa.getStates) {
        addState(k, v)
      }
    } else {
      var out = ""
      val s0 = states(0)
      for ((k, v) <- states) {
        if (v.isFinal) {
          val newDelta = mutable.Map.empty[String, Int]
          for (i <- inputSymbols) {
            newDelta += (i -> s)
          }
          v.setDelta(newDelta)
          out += v.getOutput
          v.setOutput(mutable.Set())
          v.setAsSemiFinal(true)
        }
      }

      for ((k, v) <- dfa.getStates) {
        v.shiftDelta(s)
        addState(k + s, v)
        if (v.isFinal) {
          v.setDelta(s0.getDeltaFunction)
          out += v.getOutput
          v.setOutput(mutable.Set(out))
        }
        if (v.isStart) {
          v.setAsStart(false)
          v.setAsSemiStart(true)
        }
      }

      for (i <- inputSymbols) {
        for ((k, v) <- states) {
          if (v.getDelta(i) == -1) {
            if (v.isStart) v.addDelta(i, 0)
            else if (v.isSemiStart) v.addDelta(i, k)
            else if (v.isSemiFinal) v.addDelta(i, k + 1)
            else v.addDelta(i, getPreviousSemiStart(k))
          }
        }
      }
    }
  }

  def getPreviousSemiStart(s: Int): Int = {
    var prev = 0
    for ((k, v) <- states) {
      if (k <= s & k > prev & v.isSemiStart) prev = k
    }
    prev
  }
  */
}
