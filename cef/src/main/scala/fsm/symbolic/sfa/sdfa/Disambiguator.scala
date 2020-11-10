package fsm.symbolic.sfa.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.{Guard, IdGenerator, Transition}
import fsm.symbolic.sfa.logic.Sentence
import utils.Progressor

import scala.util.control.Breaks._

/**
  * Implementation of Algorithm 1 from
  * @article{nuel2008pattern,
  *   title={Pattern Markov chains: optimal Markov chain embedding through deterministic finite automata},
  *   author={Nuel, Gr{\'e}gory},
  *   journal={Journal of Applied Probability},
  *   volume={45},
  *   number={1},
  *   pages={226--243},
  *   year={2008},
  *   publisher={Cambridge University Press}
  * }
  *
  * Adapted for symbolic automata.
  *
  *
  * Require:A= (A,Q,s,F,δ) is a (m−1)-unambiguous DFA that recognize W
  * 1:  INITIALIZATION:
  * 2:  Q0=Q, ∀q∈Q, Dq=δ−m(q) and Gq= ∆−1(q)
  * 3:  MAIN  LOOP:
  * 4:  for all q∈Q0 do
  * 5:    while |Dq|>1 do
  * 6:      take a=a1. . . am∈Dq
  * 7:      add a new state qa to Q
  * 8:      if q∈F then add qa to F
  * 9:      define Dqa ={a} and Gqa=∅
  * 10:     for all b∈A do
  *           δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
  * 11:     for all p∈Gq
  * 12:       if δ(p, am) = q and δ−(m−1)(p) =a1. . . am−1 (empty condition if m = 1) then
  * 13:         δ(p, am) = qa and add p to Gqa
  * 14:     for all p∈Gq
  *           if q /∈ δ(p,A) then
  *             remove q from Gq
  * 15:     remove a from Dq
  *
  */
class Disambiguator {

}

object Disambiguator extends LazyLogging {

  /**
    * Creates a disambiguated SDFA of order order from a given SDFA of order 0.
    *
    * @param sdfa The original SDFA.
    * @param order The disambiguation order.
    * @return The disambiguated SDFA.
    */
  private[sdfa] def disambiguate(
                                  sdfa: SDFA,
                                  order: Int
                                ): SDFA = {
    require(order >= 0)
    logger.debug("Disambiguating SDFA up to " + order + "...")
    if (order == 0) sdfa
    else {
      // Disambiguation happens incrementally. First disambiguate up to order 1, then disambiguate this SDFA up to order
      // 2, etc.
      val prevOrderSDFA = disambiguate(sdfa, order - 1)
      disambiguateK(prevOrderSDFA, order)
    }
  }

  /**
    * Creates a disambiguated SDFA of order order from a given SDFA of order order-1.
    *
    * @param sdfa The disambiguated SDFA of order order-1.
    * @param order The disambiguation order.
    * @return The disambiguated SDFA of order order.
    */
  private def disambiguateK(
                             sdfa: SDFA,
                             order: Int
                           ): SDFA = {
    require(order > 0)
    require(isMUnambiguous(sdfa, order - 1))
    // Q0=Q, ∀q∈Q, Dq=δ−m(q) and Gq= ∆−1(q)
    val Q0 = sdfa.states.keySet.toList.sorted
    val Am = buildAm(sdfa, order)
    var Dq = buildDQs(sdfa, Am)
    var Gq = buildGQs(sdfa)
    val A = sdfa.getSentences

    val initStates = sdfa.states
    val initTransitions = sdfa.transitions
    val initDuplicates = sdfa.duplicates
    val initFinals = sdfa.finals
    var states = initStates
    var transitions = initTransitions
    var duplicates = initDuplicates
    var finals = initFinals
    val q0Size = Q0.size
    var counter = 0
    val idg = IdGenerator(sdfa.states.keySet)
    var tmpSDFA = sdfa
    logger.debug("Disambiguating up to " + order + ". Scanning " + q0Size + " states...")
    val Q0It = Q0.iterator
    val progressor = Progressor("Q0", q0Size)
    while (Q0It.hasNext) { // for all q∈Q0 do
      val qk = Q0It.next()
      counter += 1
      progressor.tick
      var thisDq = Dq(qk)
      if (thisDq.isEmpty) {
        //logger.debug("Found dq with 0 size")
      }
      while (thisDq.size > 1) { // while |Dq|>1 do
        var thisGq = Gq(qk)
        // take a=a1. . . am∈Dq
        val a = thisDq.head

        // add a new state qa to Q
        val qai = idg.getId
        val qa = SDFAState(qai)
        states = states + (qai -> qa)
        val newQkDups = duplicates(qk) + qai
        duplicates = duplicates.updated(qk, newQkDups) + (qai -> Set.empty[Int])
        if (finals.contains(qk)) finals = finals + qai
        // define Dqa ={a} and Gqa=∅
        val dqa = Set(a)
        var gca: Set[Int] = Set.empty

        // for all b∈A do
        for (b <- A) {
          // δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          // Not correct. Does not take into account loop states which are m-unambiguous.
          // Should be
          //  if q is loop state on b and a = b^m (i.e., a1=b,...,am=b) then
          //    δ(qa, b) = qa and add qa to Gqa
          //  else
          //    δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          val dqb = selectNextState(tmpSDFA, qk, qai, a, b)
          val tb = Transition(qai, dqb, Guard(b))
          transitions = tb :: transitions
          if (dqb == qai) gca += qai
          else {
            //Gq(dqb) += qai
            val newg = Gq(dqb) + qai
            Gq = Gq.updated(dqb, newg)
          }
        }
        tmpSDFA = SDFA(states, transitions, sdfa.start, finals, duplicates)

        // for all p∈Gq
        for (pi <- thisGq) {
          // if δ(p, am) = q and δ−(m−1)(p) =a1. . . am−1 (empty condition if m = 1) then
          val am = a.last
          if (tmpSDFA.getDeltaOnSentence(pi, am) == qk) {
            var tmp = true
            tmp = dm1Equalsa1m1(tmpSDFA, a, order, pi)
            // δ(p, am) = qa and add p to Gqa
            if (tmp) {
              val ti = transitions.indexWhere(t => t.source == pi & t.guard.sentence == am)
              val tb = Transition(pi, qai, Guard(am))
              transitions = transitions.updated(ti, tb)
              gca += pi
              tmpSDFA = SDFA(states, transitions, sdfa.start, finals, duplicates)
            }
          }
        }

        tmpSDFA = SDFA(states, transitions, sdfa.start, finals, duplicates)
        // for all p∈Gq
        val thisGqIt = thisGq.iterator
        while (thisGqIt.hasNext) {
          val pi = thisGqIt.next()
          // if q /∈ δ(p,A) then
          val dpA = tmpSDFA.getDeltaOnSentenceSet(pi, A)
          if (!dpA.contains(qk)) {
            // remove q from Gq
            // "Remove q" is probably a mistake/typo. Should be "remove p"
            // remove p from Gq
            thisGq = thisGq - pi
          }
        }

        thisDq = thisDq - a

        Dq += (qai -> dqa)
        Gq += (qai -> gca)
        tmpSDFA = SDFA(states, transitions, sdfa.start, finals, duplicates)
      } // while (thisDq.size > 1)

      tmpSDFA = SDFA(states, transitions, sdfa.start, finals, duplicates)

    } // for (qk <- Q0)

    SDFA(states, transitions, sdfa.start, finals, duplicates)
  }

  private def dm1Equalsa1m1(
                             sdfa: SDFA,
                             a: List[Sentence],
                             m: Int,
                             p: Int
                           ): Boolean = {
    require(m > 0)
    if (m == 1) return true
    val a1am1 = Set(a.init)
    val dm1 = dmOfState(sdfa, p, m - 1)
    dm1 == a1am1
  }

  private def dmOfState(
                         sdfa: SDFA,
                         state: Int,
                         m: Int
                       ): Set[List[Sentence]] = {
    val Am = buildAm(sdfa, m)
    buildDQ(sdfa, Am, state)
  }

  private def selectNextState(
                               sdfa: SDFA,
                               q: Int,
                               qa: Int,
                               a: List[Sentence],
                               b: Sentence
                             ): Int = {
    val nextState = if (a.forall(ai => ai == b) & sdfa.isLoopStateOn(q, b)) qa
    else sdfa.getDeltaOnSentence(q, b)
    nextState
  }

  private[sdfa] def isMUnambiguous(
                                    sdfa: SDFA,
                                    m: Int
                                  ): Boolean = {
    require(m >= 0)
    val am = buildAm(sdfa, m)
    val dqs = buildDQs(sdfa, am)
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

  private def buildDQs(
                        sdfa: SDFA,
                        Am: Set[List[Sentence]]
                      ): Map[Int, Set[List[Sentence]]] = {
    val statesIds = sdfa.states.keySet
    val dqs = statesIds.map(id => (id -> buildDQ(sdfa, Am, id))).toMap
    dqs
  }

  // δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}
  private def buildDQ(
                       sdfa: SDFA,
                       Am: Set[List[Sentence]],
                       q: Int
                     ): Set[List[Sentence]] = {
    if (Am.isEmpty) Set.empty[List[Sentence]]
    else {
      val statesIds = sdfa.states.keySet
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

  private def buildGQs(sdfa: SDFA): Map[Int, Set[Int]] = {
    val statesIds = sdfa.states.keySet
    val gqs = statesIds.map(id => (id -> buildGQ(sdfa, id))).toMap
    gqs
  }

  // ∆−1 (q) = {p ∈ Q, ∃a ∈ A, δ(p, a) = q}
  private def buildGQ(
                       sdfa: SDFA,
                       q: Int
                     ): Set[Int] = {
    val statesIds = sdfa.states.keySet
    val sentences = sdfa.getSentences
    statesIds.filter(p => sentences.exists(a => sdfa.getDeltaOnSentence(p, a) == q))
  }

  // δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}
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

  private def buildGQProc(
                           sdfa: SDFA,
                           q: Int
                         ): Set[Int] = {
    val statesIds = sdfa.states.keySet
    val sentences = sdfa.getSentences
    var statesFiltered = Set.empty[Int]
    val statesit = statesIds.iterator
    while (statesit.hasNext) {
      val p = statesit.next()
      var existsToq = false
      breakable {
        val sentencesit = sentences.iterator
        while (sentencesit.hasNext) {
          val a = sentencesit.next()
          if (sdfa.getDeltaOnSentence(p, a) == q) {
            existsToq = true
            break
          }
        }
      }
      if (existsToq) {
        statesFiltered = statesFiltered + p
      }
    }
    statesFiltered
  }
}
