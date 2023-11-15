package fsm.symbolic.sfa.sdfa

import fsm.symbolic.sfa.{SFAGuard, SFATransition}
import com.typesafe.scalalogging.LazyLogging
import fsm.classical.fa.dfa.DFA
import fsm.symbolic.logic.PredicateConstructor
import fsm.CountPolicy.CountPolicy
import scala.util.control.Breaks.{break, breakable}
import fsm.symbolic.Constants.deadStateIdConstant
import fsm.CountPolicy.OVERLAP
import fsm.symbolic.logic.{AtomicSentence, Sentence}
import model.vmm.Symbol
import model.vmm.mapper.Isomorphism

object SDFAUtils extends LazyLogging {

  /**
   * Flips all states of a given SDFA, i.e., makes all of its final states non-final and all of its non-final final.
   * You have to make sure that the SDFA is complete before flipping.
   *
   * @param sdfa The original SDFA.
   * @return The same SDFA with flipped states.
   */
  def flipStates(sdfa: SDFA): SDFA = {
    val allStates = sdfa.states.keySet
    val oldFinals = sdfa.finals
    val newFinals: Set[Int] = allStates.diff(oldFinals)//allStates - oldFinals
    val newSDFA = SDFA(sdfa.states,sdfa.transitions,sdfa.start,newFinals)
    newSDFA
  }

  /**
    * Converts a classical DFA to a SDFA, according to the provided isomorphism.
    *
    * @param dfa The original, classical DFA.
    * @param iso The isomorphism, mapping each classical symbol to a minterm and vice versa.
    * @return The "equivalent" SDFA.
    */
  def dfa2sdfa(
                dfa: DFA,
                iso: Isomorphism
              ): SDFA = {
    val states = dfa.getStates.map(s => (s._1, SDFAState(s._1))).toMap
    val transitions = dfa.getTransitions.map(t => SFATransition(t.source, t.target, SFAGuard(iso.getMinTermForSymbol(Symbol(t.symbol.toInt)))))
    val start = dfa.getStart
    val finals = dfa.getAllFinals.toSet
    SDFA(states, transitions, start, finals)
  }

  /**
    * Converts a classical DFA to a SDFA. Each symbol on DFA transitions is converted to an event type predicate.
    *
    * @param dfa The original, classical DFA.
    * @return The "equivalent" SDFA.
    */
  def dfa2sdfa(dfa: DFA): SDFA = {
    val states = dfa.getStates.map(s => (s._1, SDFAState(s._1))).toMap
    val transitions = dfa.getTransitions.map(t => SFATransition(t.source, t.target, SFAGuard(AtomicSentence(PredicateConstructor.getEventTypePred(t.symbol)).asInstanceOf[Sentence])))
    val start = dfa.getStart
    val finals = dfa.getAllFinals.toSet
    SDFA(states, transitions, start, finals)
  }

  /**
    * Disambiguates a SDFA up to a specific order.
    * Uses immutable data structures.
    *
    * @param sdfa The original SDFA.
    * @param order The disambiguation order.
    * @return The disambiguated SDFA.
    */
  def disambiguateImmut(
                         sdfa: SDFA,
                         order: Int
                       ): SDFA = {
    val t1 = System.nanoTime()
    val sdfadis = Disambiguator.disambiguate(sdfa, order)
    val t2 = System.nanoTime()
    val dt = (t2 - t1) / 1000000.0
    logger.debug("Disambiguation time: " + dt)
    sdfadis
  }

  /**
    * Disambiguates a SDFA up to a specific order.
    * Uses a graph SDFA with mutable structures. Faster than
    * fsm.symbolic.sfa.sdfa.SDFAUtils#disambiguateImmut(fsm.symbolic.sfa.sdfa.SDFA, int).
    *
    * @param sdfa The original SDFA.
    * @param order The disambiguation order.
    * @return The disambiguated SDFA.
    */
  def disambiguateMutant(
                          sdfa: SDFA,
                          order: Int
                        ): SDFA = {
    val t1 = System.nanoTime()
    val disMut = DisambiguatorMutant(sdfa, order)
    val sdfadis = disMut.disambiguate
    val t2 = System.nanoTime()
    val dt = (t2 - t1) / 1000000.0
    logger.debug("Disambiguation time (mutant): " + dt + " ms.")
    logger.debug("Disambiguated SDFA has " + sdfadis.states.size + " states and " + sdfadis.transitions.size + " transitions.")
    sdfadis
  }

  /**
    * Converts a SDFA to a mutant SDFA.
    *
    * @param sdfa The original SDFA.
    * @return the mutant SDFA.
    */
  def mutate(sdfa: SDFA): SDFAMutant = SDFAMutant(sdfa)

  /**
    * Converts a SDFA to a graph SDFA.
    *
    * @param sdfa The original SDFA.
    * @return The graph SDFA.
    */
  def mutate2graph(sdfa: SDFA): SDFAMutantGraph = {
    val sdfam = mutate(sdfa)
    SDFAMutantGraph(sdfam)
  }

  /**
    * If policy is non-overlap, slight modification required.
    *
    * @param sdfa The SDFA.
    * @param policy Counting policy.
    * @return SDFA, possibly modified if non-overlap
    */
  def setPolicy(
                 sdfa: SDFA,
                 policy: CountPolicy
               ): SDFA = {
    if (policy == OVERLAP) sdfa else SDFAUtils.getNonoverlap(sdfa)
  }

  /**
    * Just checking for accessible dead states. None should exist.
    *
    * @param sdfa The SDFA
    * @return true if no dead accessible state exists.
    */
  def checkForDead(sdfa: SDFA): Boolean = {
    if (sdfa.states.exists(s => s._1 == deadStateIdConstant)) {
      logger.error("There should not be an accessible dead state at this point")
      throw new Error("There should not be an accessible dead state at this point")
    }
    true
  }

  /**
    * Converts a SDFA (which, by default, have an overlap counting policy) to a SDFA with a nonoverlap policy.
    * We do this by copying all outgoing transitions from the start state unto all the final states. That is, we replace
    * all transitions from the finals with the transitions from the start state. We thus turn final states into "start"
    * states so that the SDFA, after every match, gets reset.
    *
    * @param sdfa The original SDFA.
    * @return The SDFA with nonoverlap counting policy.
    */
  def getNonoverlap(sdfa: SDFA): SDFA = {
    val finals = sdfa.finals.toList
    resetStates(sdfa, finals)
  }

  /**
    * Resets all given states into "start" states.
    *
    * @param sdfa The original SDFA.
    * @param toReset The states to reset.
    * @return A SDFA with all given states reset.
    */
  @scala.annotation.tailrec
  private def resetStates(
                           sdfa: SDFA,
                           toReset: List[Int]
                         ): SDFA = {
    toReset match {
      case Nil => sdfa
      case head :: Nil => copyTransitions(sdfa, sdfa.start, head)
      case head :: tail => resetStates(copyTransitions(sdfa, sdfa.start, head), tail)
    }
  }

  /**
    *  Copies all outgoing transitions of the "from" state as outgoing transitions of the "to" state and deletes all
    *  other "to" transitions.
    *
    * @param sdfa The original SDFA.
    * @param from The state from which to copy transitions.
    * @param to The state to which we copy transitions.
    * @return The SDFA with copied transitions.
    */
  private def copyTransitions(
                               sdfa: SDFA,
                               from: Int,
                               to: Int
                             ): SDFA = {
    require(sdfa.states.contains(from) & sdfa.states.contains(to))
    val fromTransitions = sdfa.transitions.filter(t => t.source == from)
    val newToTransitions = fromTransitions.map(t => SFATransition(to, t.target, t.guard.asInstanceOf[SFAGuard]))
    val intactTransitions = sdfa.transitions.filter(t => t.source != to)
    val newTransitions = newToTransitions ::: intactTransitions
    SDFA(sdfa.states, newTransitions, sdfa.start, sdfa.finals)
  }

  /**
    * Checks whether a SDFA is m-unambiguous.
    *
    * @param sdfa The given SDFA to check.
    * @param m The order.
    * @return True if the SDFA is m-unabmiguous.
    */
  def isMUnambiguous(
                      sdfa: SDFA,
                      m: Int
                    ): Boolean = {
    require(m >= 0)
    val am = buildAm(sdfa, m)
    val dqs = buildDQs(sdfa, am)
    // condition should be size!=1, but initial state may result without any incoming edges
    //!dqs.exists(dq => dq._2.size > 1)
    val dqsit = dqs.iterator
    var isUnamb = true
    breakable {
      while (dqsit.hasNext) {
        val dq = dqsit.next()
        if (dq._2.size > 1) {
          isUnamb = false
          logger.warn("SDFA not " + m + "unambiguous\n" + sdfa.toString + "\n" + dq)
          break
        }
      }
    }
    isUnamb
  }

  /**
    * Builds all permutations of length m of the sentences of a given SDFA.
    *
    * @param sdfa The SDFA from which to get the sentences.
    * @param m The length m.
    * @return Permutations of length m of the SDFA sentences.
    */
  private def buildAm(
                       sdfa: SDFA,
                       m: Int
                     ): Set[List[Sentence]] = {
    require(m >= 0)
    if (m == 0) Set.empty
    else {
      val sentences = sdfa.getSentences
      utils.SetUtils.permutations(sentences, m)
    }
  }

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q} for all states q.
    *
    * @param sdfa The given SDFA.
    * @param Am Permutations of length m of the sentences of the given SDFA
    * @return Dq for all q
    */
  private def buildDQs(
                        sdfa: SDFA,
                        Am: Set[List[Sentence]]
                      ): Map[Int, Set[List[Sentence]]] = {
    val statesIds = sdfa.states.keySet
    val dqs = statesIds.map(id => (id -> buildDQ(sdfa, Am, id))).toMap
    dqs
  }

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q} for a given state q.
    *
    * @param sdfa The given SDFA.
    * @param Am Permutations of length m of the sentences of the given SDFA.
    * @param q The given state q.
    * @return Dq for q.
    */
  private def buildDQ(
                       sdfa: SDFA,
                       Am: Set[List[Sentence]],
                       q: Int
                     ): Set[List[Sentence]] = {
    if (Am.isEmpty) Set.empty[List[Sentence]]
    else {
      val statesIds = sdfa.states.keySet
      //Am.filter(a => statesIds.exists(id => sdfa.getDeltaOnSentence(id, a) == q))
      var AmFiltered = Set.empty[List[Sentence]]
      val Amit = Am.iterator
      while (Amit.hasNext) {
        val a = Amit.next()
        var existsToq = false
        breakable {
          val statesit = statesIds.iterator
          while (statesit.hasNext) {
            val id = statesit.next()
            if (sdfa.getDeltaOnSentence(id, a) == q) {
              existsToq = true
              break
            }
          }
        }
        if (existsToq) {
          AmFiltered = AmFiltered + a
        }
      }
      AmFiltered
    }
  }

  /**
    * Builds Dq = δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q} for a given state q.
    * Not very efficient.
    * Use fsm.symbolic.sfa.sdfa.SDFAUtils#buildDQ(fsm.symbolic.sfa.sdfa.SDFA, scala.collection.immutable.Set, int).
    *
    * @param sdfa The given SDFA.
    * @param Am Permutations of length m of the sentences of the given SDFA.
    * @param q The given state q.
    * @return Dq for q.
    */
  private def buildDQSca(
                          sdfa: SDFA,
                          Am: Set[List[Sentence]],
                          q: Int
                        ): Set[List[Sentence]] = {
    if (Am.isEmpty) Set.empty[List[Sentence]]
    else {
      val statesIds = sdfa.states.keySet
      Am.filter(a => statesIds.exists(id => sdfa.getDeltaOnSentence(id, a) == q))
    }
  }
}
