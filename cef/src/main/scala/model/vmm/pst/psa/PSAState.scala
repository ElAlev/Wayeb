package model.vmm.pst.psa

import breeze.stats.distributions.Uniform
import utils.StringUtils
import model.vmm.{Symbol, SymbolWord}

object PSAState {
  /**
    * Constructor for PSAStates.
    *
    * @param label The state's label as a SymbolWord.
    * @param tr The state's outgoing transitions.
    * @return A PSA state.
    */
  def apply(
             label: SymbolWord,
             tr: Map[Symbol, PSATransition]
           ): PSAState = new PSAState(label, tr)

  /**
    * Constructor for PSAStates without any outgoing transitions.
    *
    * @param label The state's label as a SymbolWord.
    * @return A PSA state.
    */
  def apply(label: SymbolWord): PSAState = new PSAState(label, Map.empty[Symbol, PSATransition])
}

/**
  * States of probabilistic suffix automata.
  *
  * @param label The state's label as a SymbolWord.
  * @param tr The state's outgoing transitions.
  */
class PSAState(
                val label: SymbolWord,
                tr: Map[Symbol, PSATransition]
              ) extends Serializable {

  private var transitions: Map[Symbol, PSATransition] = tr
  private var cumulativeDist: List[(Symbol, Double)] =
    if (transitions.nonEmpty) caclulateCumulativeDistribution()
    else List.empty

  /**
    * Finds the next state reached with a given symbol.
    *
    * @param symbol The given symbol.
    * @return The next state, along with the probability of the triggered transition.
    */
  def getNext(symbol: Symbol): (PSAState, Double) = {
    require(transitions.contains(symbol))
    val transition = transitions(symbol)
    (transition.target, transition.prob)
  }

  /**
    * Finds the probability of the transition triggered with a given symbol.
    *
    * @param symbol The given symbol.
    * @return The transition's probability.
    */
  def getProbFor(symbol: Symbol): Double = {
    require(transitions.contains(symbol))
    val transition = transitions(symbol)
    transition.prob
  }

  /**
    * Given the label of another state, finds the probability of the transition that leads us to that state. If no such
    * transition exists, returns 0.0.
    *
    * @param to The other state's label.
    * @return The transition's probability.
    */
  def getProbTo(to: SymbolWord): Double = {
    transitions.find(t => t._2.target.label == to) match {
      case Some(x) => x._2.prob
      case _ => 0.0
    }
  }

  /**
    * Sets the transitions of the state. Previous transitions (if they exist) are overwritten.
    *
    * @param ns The transitions to next states, as a map of symbols to PSA transitions.
    */
  def setTransitionsToNextStates(ns: Map[Symbol, PSATransition]): Unit = {
    transitions = ns
    cumulativeDist = caclulateCumulativeDistribution()
  }

  /**
    * @return All transitions as a string.
    */
  private def nextStates2String: String = {
    val strings = transitions.map(s => "Next for :" + s._1.toString + "->" + s._2.toString)
    StringUtils.list2Str(strings.toList)
  }

  /**
    * @return The symbols from all transitions.
    */
  def getSymbols: Set[Symbol] = transitions.keySet

  /**
    * Generates a symbol according to the state's next symbol distribution.
    *
    * @return A symbol and the label of the state reached with the generated symbol.
    */
  def generateSymbol(): (Symbol, SymbolWord) = {
    // first, generate a sample from a uniform distribution
    val uni = Uniform(0, 1)
    val s = uni.sample()
    // then find where this sample falls in bins of the symbols' cumulative distribution
    val symbol = generateSymbolAux(s, cumulativeDist)
    val nextStateLabel = getNext(symbol)._1.label
    (symbol, nextStateLabel)
  }

  /**
    * Auxiliary recursive function to generate a symbol. Scans the distribution to find the bin where the given sample
    * falls and returns the corresponding symbol.
    *
    * @param sample The given sample.
    * @param dist The cumulative distribution.
    * @return The selected symbol.
    */
  @scala.annotation.tailrec
  private def generateSymbolAux(
                                 sample: Double,
                                 dist: List[(Symbol, Double)]
                               ): Symbol = {
    if (dist.length == 1) dist.head._1
    else {
      if (sample < dist.head._2) dist.head._1
      else generateSymbolAux(sample, dist.tail)
    }
  }

  /**
    * @return The next symbol cumulative distribution.
    */
  private def caclulateCumulativeDistribution(): List[(Symbol, Double)] = {
    require(transitions.nonEmpty)
    var dist = transitions.map(t => (t._1, t._2.prob)).toList
    var cumulativeDist: List[(Symbol, Double)] = List.empty
    var cumulativeProb = 0.0
    while (dist.nonEmpty) {
      val prob = dist.head._2
      cumulativeProb += prob
      cumulativeDist = (dist.head._1, cumulativeProb) :: cumulativeDist
      dist = dist.tail
    }
    cumulativeDist = cumulativeDist.reverse
    cumulativeDist
  }

  override def toString: String = "Label: " + label + " \t Next States: " + nextStates2String
}
