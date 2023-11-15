package model.vmm.pst.psa

import breeze.linalg.{DenseMatrix, eig}
import stream.GenericEvent
import stream.array.EventStream
import utils.{Progressor, StringUtils}
import model.vmm.{Symbol, SymbolWord}
import scala.collection.immutable.SortedMap

object ProbSuffixAutomaton {
  /**
    * Constructor for PSA.
    *
    * @param states The states of the PSA as a map of state labels to state objects.
    * @return A PSA.
    */
  def apply(states: Map[SymbolWord, PSAState]): ProbSuffixAutomaton = new ProbSuffixAutomaton(states)
}

/**
  * See
  * @article{DBLP:journals/ml/RonST96,
  *                                   author    = {Dana Ron and
  *                                   Yoram Singer and
  *                                   Naftali Tishby},
  *                                   title     = {The Power of Amnesia: Learning Probabilistic Automata with Variable
  *                                   Memory Length},
  *                                   journal   = {Mach. Learn.},
  *                                   volume    = {25},
  *                                   number    = {2-3},
  *                                   pages     = {117--149},
  *                                   year      = {1996}
  *                                   }
  *
  * A Probabilistic Suffix Automaton M is a tuple (Q, Σ, τ, γ, π), where:
  *   Q is a finite set of states;
  *   Σ is a finite alphabet;
  *   τ : Q × Σ → Q is the transition function;
  *   γ : Q × Σ → [0, 1] is the next symbol probability function;
  *   π : Q → [0, 1] is the initial probability distribution over the starting states;
  * The following conditions must hold:
  *   For every q ∈ Q, it must hold that Sumγ(q, σ) = 1 and Sumπ(q) = 1;
  *   Each q ∈ Q is labeled by a string s ∈ Σ∗ and the set of labels is suffix free, i.e., no label s
  *     is a suffix of another label s0 ;
  *   For every two states q1 , q2 ∈ Q and for every symbol σ ∈ Σ, if τ (q1 , σ) = q2 and q1 is
  *     labeled by s1 , then q2 is labeled by s2, such that s2 is a suffix of s1 · σ;
  *   For every s labeling some state q, and every symbol σ for which γ(q, σ) > 0, there exists
  *     a label which is a suffix of s · σ;
  *   Finally, the graph of M is strongly connected.
  *
  * @param states The states of the PSA as a map of state labels to state objects.
  */
class ProbSuffixAutomaton(val states: Map[SymbolWord, PSAState]) extends Serializable {

  /**
    * @return The transition matrix of the PSA.
    */
  def getTransitionMatrix: PSAMatrix = {
    val matrix = DenseMatrix.zeros[Double](size, size)
    val state2row: Map[SymbolWord, Int] = states.keySet.zipWithIndex.toMap
    val row2State: Map[Int, SymbolWord] = state2row.map(_.swap)
    for (r <- 0 until matrix.rows; c <- 0 until matrix.cols) {
      matrix(r, c) = getProbFromTo(row2State(r), row2State(c))
    }
    PSAMatrix(matrix, state2row, row2State)
  }

  /**
    * @return The number of the PSA's states.
    */
  def size: Int = states.size

  /**
    * @return The PSA's order, i.e., the maximum length of the PSA's labels.
    */
  def maxOrder: Int = states.keySet.map(s => s.length).max

  /**
    * Finds the probability of the transition from a given state to another given state. 0.0 if no such transition
    * exists. Assumes that both states exist.
    *
    * @param from The label of the given source state.
    * @param to The label of the given target state.
    * @return The transition's probability.
    */
  def getProbFromTo(
                     from: SymbolWord,
                     to: SymbolWord
                   ): Double = {
    require(states.contains(from) & states.contains(to))
    val fromState = states.find(s => s._1 == from) match {
      case Some(x) => x
      case _ => throw new Error("Could not find from state!")
    }
    fromState._2.getProbTo(to)
  }

  /**
    * @return The number of states per order along with the percentage the states for each order (among all states).
    */
  def statesByOrder: Map[Int, (Int, Double)] = {
    // a list with the order of every state
    val orders = states.keySet.toList.map(s => s.length)
    // the orders grouped
    val ordersGrouped = orders.groupBy(o => o)
    // now, for each group,
    // count its members (find the number of states with that order)
    // and calculate the percentage of the states with this order (among all states)
    val ordersCounts = ordersGrouped.mapValues(v => (v.size, v.size.toDouble / size))
    // sort by order
    SortedMap(ordersCounts.toSeq: _*)
  }

  /**
    * Finds the PSA state corresponding to a given label. Assumes the state exists.
    *
    * @param label The state's label.
    * @return The PSA state.
    */
  def getStateForLabel(label: SymbolWord): PSAState = {
    require(states.contains(label))
    states(label)
  }

  /**
    * Checks whether there exists a state with the given label.
    *
    * @param label The given label.
    * @return True if such a state exists.
    */
  def hasStateForLabel(label: SymbolWord): Boolean = states.contains(label)

  /**
    * Checks whether the PSA can start working with the given word. There must exist a state whose label is a suffix of
    * the given word.
    *
    * @param word The given word.
    * @return The state with which we can start (its label and the state itself). None if no such state exists.
    */
  def canStartWith(word: SymbolWord): Option[(SymbolWord, PSAState)] = states.find(s => s._1.isSuffixOf(word))

  /**
    * finds the state we can reach from a given state with a given symbol. Assumes given state exists.
    *
    * @param fromStateLabel The label of the given state.
    * @param withSymbol The given symbol.
    * @return The state reached along with the probability of the triggered transition.
    */
  def nextState(
                 fromStateLabel: SymbolWord,
                 withSymbol: Symbol
               ): (PSAState, Double) = {
    require(states.contains(fromStateLabel))
    val fromState = states(fromStateLabel)
    fromState.getNext(withSymbol)
  }

  /**
    * @return The labels from all states.
    */
  def getLabels: Set[SymbolWord] = states.keySet

  /**
    * @return All symbols present in the PSA.
    *         TODO: assumes that all states can follow the same set of symbols, so we can just get the symbols of any state (unit test needed)
    */
  def getSymbols: Set[Symbol] = states.head._2.getSymbols

  /**
    * Generates a stream by using the PSA as a generator.
    * The PSA can act as a generator of strings. It can use π, its initial distribution on states, to select an initial
    * state and generate its label as a first string and then continuously use γ to generate a symbol, move to a next
    * state and repeat the same process. At every time, the label of its state is always a suffix of the string
    * generated thus far.
    *
    * @param eventsNo The number of events to be generated.
    * @return The event stream along with a map from orders to percentage of labels generated by each order.
    */
  def generateStream(eventsNo: Int): (EventStream, Map[Int, Double]) = {
    val symbolsList = getSymbols.toList
    val symbolStrings: List[String] = symbolsList.map(_.toString)
    val symbols2types = symbolsList.zip(symbolStrings).toMap
    generateStream(eventsNo, symbols2types)
  }

  /**
    * Generates a stream by using the PSA as a generator.
    * The PSA can act as a generator of strings. It can use π, its initial distribution on states, to select an initial
    * state and generate its label as a first string and then continuously use γ to generate a symbol, move to a next
    * state and repeat the same process. At every time, the label of its state is always a suffix of the string
    * generated thus far.
    *
    * @param eventsNo The number of events to be generated.
    * @param symbols2types A map of PSA symbols to strings. The event type for each generated symbol will be retrieved
    *                      from this map.
    * @return The event stream along with a map from orders to percentage of labels generated by each order.
    */
  def generateStream(
                      eventsNo: Int,
                      symbols2types: Map[Symbol, String]
                    ): (EventStream, Map[Int, Double]) = {
    require(eventsNo > maxOrder)
    val eventStream = new EventStream()
    val startState = states.head
    var initWord = startState._1
    var eventIndex = 0
    var counters = Map.empty[String, Int]
    var stateCounters: Map[SymbolWord, Int] = getLabels.map(label => (label, 0)).toMap
    while (initWord.nonEmpty) {
      val symbol = initWord.head
      eventIndex += 1
      val newEvent = GenericEvent(symbols2types(symbol), eventIndex)
      eventStream.addEvent(newEvent)
      if (counters.contains(symbol.toString)) counters = counters + (symbol.toString -> (counters(symbol.toString) + 1))
      else counters = counters + (symbol.toString -> 1)
      initWord = initWord.tail
    }
    var currentState = startState._2
    stateCounters = stateCounters + (currentState.label -> (stateCounters(currentState.label) + 1))
    val progressor = Progressor("PSA Stream", eventsNo)
    while (eventIndex < eventsNo) {
      val (symbol, nextStateLabel) = currentState.generateSymbol()
      eventIndex += 1
      val newEvent = GenericEvent(symbols2types(symbol), eventIndex)
      eventStream.addEvent(newEvent)
      if (counters.contains(symbol.toString)) counters = counters + (symbol.toString -> (counters(symbol.toString) + 1))
      else counters = counters + (symbol.toString -> 1)
      currentState = states(nextStateLabel)
      stateCounters = stateCounters + (currentState.label -> (stateCounters(currentState.label) + 1))
      progressor.tick
    }
    eventStream.setCounters(counters)
    val countersByOrder = stateCounters.groupBy(s => s._1.length).mapValues(e => e.values.sum)
    val totalVisits = countersByOrder.values.sum
    val percentByOrder = countersByOrder.mapValues(v => v.toDouble / totalVisits)
    val percentNonExisting = ((1 to maxOrder).toSet &~ percentByOrder.keySet).map(s => (s, 0.0)).toMap
    (eventStream, percentByOrder ++ percentNonExisting)
  }

  override def toString: String = {
    val sortedStates = SortedMap(states.toSeq: _*)
    StringUtils.list2Str(sortedStates.values.map(x => x.toString).toList, "\n")
  }

}
