package model.vmm.pst.spsa

import fsm.symbolic.Valuation

import java.io.{FileOutputStream, ObjectOutputStream}
import fsm.symbolic.sfa.{SFAGuard, SFATransition}
import fsm.symbolic.sfa.sdfa.SDFA
import model.vmm.mapper.Isomorphism
import stream.GenericEvent
import model.vmm.{Symbol, SymbolWord}

object SymbolicPSA {
  /**
    * Constructor for SPSA.
    *
    * @param states The SPSA states.
    * @param iso The isomorphism.
    * @return A SPSA.
    */
  def apply(
             states: Map[(Int, SymbolWord), SPSAState],
             iso: Isomorphism
           ): SymbolicPSA = new SymbolicPSA(states, iso)
}

/**
  * This class represents a symbolic probabilistic suffix automaton, i.e., an embedding of a PSA in a SDFA.
  * Each SPSA state is constructed by combining a state from the SDFA with a state from the PSA.
  * Most of the necessary information (e.g., transitions) is held by the states themselves (in contrast to
  * other automata, like DFA and SDFA). This is why we do not need many structures here. We do need the isomorphism
  * that maps minterms to symbols and vice versa so that we can evaluate transitions on events.
  *
  * Let MR be a DSFA (its mapping to a classical automaton) and MS a PSA with the same alphabet.
  * An embedding of MS in MR is a tuple M = (Q, Qs, Qf , Σ, ∆, Γ), where:
  *   Q is a finite set of states;
  *   Qs ⊆ Q is the set of initial states;
  *   Qf ⊆ Q is the set of final states;
  *   Σ is a finite alphabet;
  *   ∆ : Q × Σ → Q is the transition function;
  *   Γ : Q × Σ → [0, 1] is the next symbol probability function;
  *   π : Q → [0, 1] is the initial probability distribution.
  * The language L(M ) of M is defined, as usual, as the set of strings that lead M to a final state.
  * The following conditions must hold:
  *   Σ = MR .Σ = MS .Σ;
  *   L(M ) = L(MR );
  *   For every string/stream S1..k , PM (S1..k ) = PMS (S1..k ), where PM denotes the probability of a string
  *   calculated by M (through Γ) and PMS the probability calculated by MS (through γ).
  *
  * @param states The SPSA states.
  * @param iso The isomorphism.
  */
class SymbolicPSA(
                   val states: Map[(Int, SymbolWord), SPSAState],
                   val iso: Isomorphism
                 ) extends Serializable {

  private val start: Set[(Int, SymbolWord)] = states.filter(s => s._2.isStart).keySet
  private val finals: Set[(Int, SymbolWord)] = states.filter(s => s._2.isFinal).keySet

  /**
    * @return The number of SPSA states.
    */
  def getSize: Int = states.size

  /**
   * @return The number of unique words.
   */
  def getNoOfWords: Int = states.map(s => s._1._2).toSet.size

  /**
    * Returns the maximum order of the SPSA. The maximum order is defined as the maximum length of all the states'
    * labels. It determines how deep into the past the SPSA can look.
    *
    * @return The maximum order.
    */
  def maxOrder: Int = states.keySet.map(s => s._2.length).max

  /**
    * @return All final states as a set of ids and labels.
    */
  def getFinals: Set[(Int, SymbolWord)] = finals

  /**
    * Checks whether a state is final.
    *
    * @param idLabel the id and label of the state.
    * @return True if the state is final.
    */
  def isFinal(idLabel: (Int, SymbolWord)): Boolean = finals.contains(idLabel)

  /**
    * @return All pairs of ids and labels.
    */
  def getIdsLabels: Set[(Int, SymbolWord)] = states.keySet

  /**
    * @return All start states as a set of ids and labels.
    */
  def getStartStates: Set[(Int, SymbolWord)] = start

  /**
    * Returns true if the SPSA accepts a given word.
    *
    * @param word The word to check.
    * @return True if the word is accepted.
    */
  def accepts(word: SymbolWord): Boolean = {
    require(word.nonEmpty)
    val reachedState = delta(states(start.head), word)
    finals.contains((reachedState.sfaId, reachedState.psaLabel))
  }

  /**
    * Returns true if the SPSA can start with a given word.
    * A SPSA can have multiple start states and each start state can have its own label. In order to find the start
    * state from which the SPSA can begin, we need to make sure that there exists one start start state whose label
    * is a suffix of the given word. This is necessary because, during the construction of the PSA from which we build
    * the SPSA, we do not include any transient states. Is is an unnecessary overhead, since in a streaming setting,
    * the PSA quickly moves to a recurrent class. We can then choose to simply wait for a couple of symbols/events and
    * then start the SPSA, as soon as we find a start state that is proper (i.e., is a suffix of the stream).
    *
    * @param word The given word.
    * @return True if the SPSA can start with the given word.
    */
  def canStartWith(word: SymbolWord): Option[((Int, SymbolWord), SPSAState)] = {
    val startStates = states.filter(s => start.contains(s._1))
    val starting = startStates.find(s => s._2.psaLabel.isSuffixOf(word))
    starting
  }

  /**
    * Get the transition probability between two states.
    *
    * @param fromIdLabel The source state (id,label).
    * @param toIdLabel The target state (id,label).
    * @return The transition probability.
    */
  def getProbFromTo(
                     fromIdLabel: (Int, SymbolWord),
                     toIdLabel: (Int, SymbolWord)
                   ): Double = {
    require(states.contains(fromIdLabel) & states.contains(toIdLabel))
    val fromState = states(fromIdLabel)
    fromState.getProbToState(toIdLabel)
  }

  /**
    * Returns true if there exists a transition between two given states,
    *
    * @param fromIdLabel The source state (id,label).
    * @param toIdLabel The target state (id,label).
    * @return True if there exists a transition from the source to the target state.
    */
  def connected(
                 fromIdLabel: (Int, SymbolWord),
                 toIdLabel: (Int, SymbolWord)
               ): Boolean = {
    val fromStates = states.filter(s => s._1 == fromIdLabel).values.toList
    require(fromStates.size == 1)
    val fromState = fromStates.head
    fromState.connected(toIdLabel)
  }

  /**
    * Returns the state reached from a source state with a given word/sequence of symbols.
    *
    * @param fromState The source state.
    * @param withWord The symbol word.
    * @return The state reached.
    */
  @scala.annotation.tailrec
  private def delta(
                     fromState: SPSAState,
                     withWord: SymbolWord
                   ): SPSAState = {
    if (withWord.isEmpty) fromState
    else {
      val nextState = delta(fromState, withWord.head)
      delta(nextState, withWord.tail)
    }
  }

  /**
    * Returns the state reached from a source state with a given word/sequence of symbols.
    * The source state is given as an id/label tuple.
    *
    * @param fromIdLabel The id/label of the source state.
    * @param withSymbol The given symbol.
    * @return The state reached.
    */
  def delta(
             fromIdLabel: (Int, SymbolWord),
             withSymbol: Symbol
           ): (Int, SymbolWord) = {
    val fromState = states(fromIdLabel)
    val nextState = delta(fromState, withSymbol)
    (nextState.sfaId, nextState.psaLabel)
  }

  /**
    * Returns the target state from a given source state and with a given symbol.
    *
    * @param fromState The source state.
    * @param withSymbol The given symbol
    * @return The target state.
    */
  def delta(
             fromState: SPSAState,
             withSymbol: Symbol
           ): SPSAState = {
    fromState.delta(withSymbol)
  }

  /**
    * Returns the id/label of a target state from a given source state (also given as id/label tuple), with a given
    * input event.
    *
    * @param fromIdLabel The id/label of the source state.
    * @param event The input event.
    * @return The target state.
    */
  def delta(
             fromIdLabel: (Int, SymbolWord),
             event: GenericEvent
           ): (Int, SymbolWord) = {
    val withSymbol = iso.evaluate(event)
    delta(fromIdLabel, withSymbol)
  }

  /**
    * Extracts a SDFA from the SPSA, by dropping the probabilities and providing new ids to each state (Ints).
    *
    * @return The extracted SDFA, a list of the new SDFA state ids, a mapping from the id/labels of the SPSA to the ids
    *         of the SDFA and the inverse mapping.
    */
  def toSDFA: (SDFA, List[Int], Map[(Int, SymbolWord), Int], Map[Int, (Int, SymbolWord)]) = {
    val labels = getIdsLabels
    val sdfaStateIds = (1 to getSize).toList
    val id2label = sdfaStateIds.zip(labels).toMap
    val label2id = labels.zip(sdfaStateIds).toMap
    val sdfaTransitions = labels.flatMap(l => toSDFATransitions(l, label2id)).toList
    val sdfaStartStates = start.map(s => label2id(s))
    val sdfaFinalStates = finals.map(s => label2id(s))
    val sdfa = SDFA(sdfaStateIds.toSet, sdfaTransitions, sdfaStartStates.head, sdfaFinalStates)
    (sdfa, sdfaStateIds, label2id, id2label)
  }

  /**
    * Converts the outgoing transitions from a given SPSA state to SDFA transitions.
    *
    * @param idLabel The given source SPSA state (id, label).
    * @param idLabel2SDFAids Map from (id,label) of SPSA to SDFA state ids.
    * @return The SDFA transitions.
    */
  private def toSDFATransitions(
                                 idLabel: (Int, SymbolWord),
                                 idLabel2SDFAids: Map[(Int, SymbolWord), Int]
                               ): Set[SFATransition] = {
    val spsaSourceState = states(idLabel)
    val sdfaSourceStateId = idLabel2SDFAids(idLabel)
    val spsaTransitions = spsaSourceState.getTransitions
    val sdfaTransitions = spsaTransitions.map(x => {
      val sentence = iso.getMinTermForSymbol(x._1)
      val targetLabel = (x._2.target.sfaId, x._2.target.psaLabel)
      val targetSdfaStateId = idLabel2SDFAids(targetLabel)
      val sdfaTransition = SFATransition(sdfaSourceStateId, targetSdfaStateId, SFAGuard(sentence))
      sdfaTransition
    })
    sdfaTransitions.toSet
  }

  /**
    * Serializes the SPSA and writes it to a file.
    * CAUTION: serialization may fail because the SPSA is represented as a linked data structure (SPSA states have
    * references to other SPSA states).
    *
    * @param fn The name of the file.
    */
  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

  override def toString: String = {
    states.foldLeft("")(
      (x, y) =>
        x +
          "\n" +
          "ID: " +
          y._1._1 +
          " LABEL: " +
          y._1._2.toString +
          "\nSTATES\n" +
          y._2.toString
    )
  }
}
