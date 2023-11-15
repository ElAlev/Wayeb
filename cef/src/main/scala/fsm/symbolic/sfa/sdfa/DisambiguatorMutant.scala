package fsm.symbolic.sfa.sdfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.Sentence
import fsm.symbolic.sfa.{IdGenerator, SFAGuard, SFATransition}
import utils.Progressor

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

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
  * For efficiency reasons, this version uses mutant SDFAs. It's more efficient to find previous and next states using a
  * graph SDFA.
  *
  */

object DisambiguatorMutant {
  def apply(
             sdfa: SDFA,
             order: Int
           ): DisambiguatorMutant = new DisambiguatorMutant(sdfa, order)
}

/**
  *
  * @param sdfa sdfa The original SDFA.
  * @param order The disambiguation order.
  */
class DisambiguatorMutant(
                           sdfa: SDFA,
                           order: Int
                         ) extends LazyLogging {
  require(order >= 0)
  val Ams: Map[Int, Set[List[Sentence]]] = buildAms

  /**
    * Creates the disambiguates SDFA.
    *
    * @return The disambiguates SDFA
    */
  private[sdfa] def disambiguate: SDFA = {
    // first create a mutant SDFA with a graph representation and then use this for disambiguation
    val sdfam = SDFAMutant(sdfa)
    val sdfamDis = disambiguate(sdfam, order)
    // after disambiguation, convert the mutant SDFA back to a standard SDFA
    sdfamDis.toSDFA
  }

  /**
    * Creates a disambiguated SDFA of order order from a given mutant SDFA of order 0.
    *
    * @param sdfam The original mutant SDFA.
    * @param order The disambiguation order.
    * @return The disambiguated mutant SDFA.
    */
  private def disambiguate(
                            sdfam: SDFAMutant,
                            order: Int
                          ): SDFAMutant = {
    var prevOrderSDFAm = sdfam
    var Dq_1 = Map[Int, Set[List[Sentence]]]()
    // Disambiguation happens incrementally. First disambiguate up to order 1, then disambiguate this SDFA up to order
    // 2, etc.
    for (im <- 1 to order) {
      val res = disambiguateK(prevOrderSDFAm, im, Dq_1)
      val thisOrderSDFAm = res._1
      val Dq = res._2
      prevOrderSDFAm = thisOrderSDFAm
      Dq_1 = Dq
    }
    prevOrderSDFAm
  }

  /**
    * Creates a disambiguated SDFA of order order from a given SDFA of order order-1.
    *
    * @param sdfam The disambiguated SDFA of order order-1.
    * @param order The disambiguation order.
    * @param Dq_1_im The Dqs of order order-1.
    * @return The disambiguated SDFA of order order and the Dqs of order order.
    */
  private def disambiguateK(
                             sdfam: SDFAMutant,
                             order: Int,
                             Dq_1_im: Map[Int, Set[List[Sentence]]]
                           ): (SDFAMutant, Map[Int, Set[List[Sentence]]]) = {
    require(order > 0)
    logger.info("Disambiguating now up to " + order + "...")
    val Dq_1 = collection.mutable.HashMap(Dq_1_im.toSeq: _*)
    var Dq = buildDQsFromGraph(sdfam, order)
    logger.whenDebugEnabled {
      //require(SDFAUtils.isMUnambiguous(sdfam.toSDFA,m-1))
    }
    var Gq = buildGQsFromGraph(sdfam)
    val A = sdfam.sentences
    val Q0 = sdfam.states.keySet.toList.sorted

    val q0Size = Q0.size
    val idg = IdGenerator(sdfam.states.keySet.toSet)

    logger.debug("Scanning " + q0Size + " states...")
    val progressorQ0 = Progressor("Q0", q0Size)
    val Q0It = Q0.iterator
    while (Q0It.hasNext) { // for all q∈Q0 do
      val qk = Q0It.next()
      progressorQ0.tick
      var thisDq = Dq(qk)
      val initDqSize = thisDq.size
      val progressorDq = Progressor("Dq", initDqSize)
      while (thisDq.size > 1) { // while |Dq|>1 do
        //progressorDq.tick
        var thisGq = Gq(qk)
        // take a=a1. . . am∈Dq
        val a = thisDq.head

        // add a new state qa to Q
        val qai = idg.getIdCautiousImmut
        val qa = SDFAState(qai)
        sdfam.addDuplicate(qk, qai)
        if (sdfam.finals.contains(qk)) sdfam.addFinal(qai)
        // define Dqa ={a} and Gqa=∅
        val dqa = Set(a)
        var gca: Set[Int] = Set.empty

        val newTransitions = ListBuffer[SFATransition]()
        val progressorBA = Progressor("bA", A.size, 20)
        for (b <- A) { // for all b∈A do
          //progressorBA.tick
          // δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          // Not correct. Does not take into account loop states which are m-unambiguous.
          // Should be
          //  if q is loop state on b and a = b^m (i.e., a1=b,...,am=b) then
          //    δ(qa, b) = qa and add qa to Gqa
          //  else
          //    δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          val dqb = selectNextState(sdfam, qk, qai, a, b)
          val tb = SFATransition(qai, dqb, SFAGuard(b))
          newTransitions += tb
          if (dqb == qai) gca += qai
          else {
            val newg = Gq(dqb) + qai
            Gq = Gq.updated(dqb, newg)
          }
        }
        sdfam.addState(qai, qa, newTransitions.toList)

        val progressorPGQ = Progressor("PGQ", thisGq.size, 20)
        for (pi <- thisGq) { // for all p∈Gq
          //progressorPGQ.tick
          // if δ(p, am) = q and δ−(m−1)(p) =a1. . . am−1 (empty condition if m = 1) then
          val am = a.last
          if (sdfam.getDeltaOnSentence(pi, am) == qk) {
            var tmp = true
            tmp = dm1Equalsa1m1(sdfam, Dq_1, a, order, pi)
            // δ(p, am) = qa and add p to Gqa
            if (tmp) {
              gca += pi
              sdfam.updateTransition(pi, am, qai)
            }
          }
        }

        val thisGqIt = thisGq.iterator
        val progressorGQ = Progressor("GQ", thisGq.size, 20)
        while (thisGqIt.hasNext) { // for all p∈Gq
          //progressorGQ.tick
          val pi = thisGqIt.next()
          // if q /∈ δ(p,A) then
          val dpA = sdfam.getDeltaOnSentenceSet(pi, A)
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
      } // while (thisDq.size > 1)
    }
    (sdfam, Dq)
  }

  private def selectNextState(
                               sdfam: SDFAMutant,
                               q: Int,
                               qa: Int,
                               a: List[Sentence],
                               b: Sentence
                             ): Int = {
    val nextState = if (a.forall(ai => ai == b) & sdfam.isLoopStateOn(q, b)) qa
    else sdfam.getDeltaOnSentence(q, b)
    nextState
  }

  private def dm1Equalsa1m1(
                             sdfam: SDFAMutant,
                             dqs_1: collection.mutable.HashMap[Int, Set[List[Sentence]]],
                             a: List[Sentence],
                             m: Int,
                             p: Int
                           ): Boolean = {
    require(m > 0)
    if (m == 1) return true
    val a1am1 = Set(a.init)
    //val dm1 = buildDQ(sdfam,Ams(m-1),p)
    val dm1 = sdfam.buildDQ(p, m - 1)
    dm1 == a1am1
  }

  private def buildAms: Map[Int, Set[List[Sentence]]] = {
    if (order == 0) Map[Int, Set[List[Sentence]]]()
    else {
      val sentences = sdfa.getSentences
      val ams = utils.SetUtils.permutationsAlt(sentences, order)
      ams
    }
  }

  private def buildDQsFromGraph(
                                 sdfam: SDFAMutant,
                                 m: Int
                               ): Map[Int, Set[List[Sentence]]] = {
    val statesIds = sdfam.states.keySet.toList
    val dqs = statesIds.map(id => buildDQFromGraph(sdfam, id, m))
    statesIds.zip(dqs).toMap
  }

  // δ−m (q) = {a ∈ Am , ∃p ∈ Q, δ(p, a) = q}
  private def buildDQFromGraph(
                                sdfam: SDFAMutant,
                                q: Int,
                                m: Int
                              ): Set[List[Sentence]] = sdfam.buildDQ(q, m)

  private def buildDQ(
                       sdfam: SDFAMutant,
                       Am: Set[List[Sentence]],
                       q: Int
                     ): Set[List[Sentence]] = {
    if (Am.isEmpty) Set.empty[List[Sentence]]
    else {
      val statesIds = sdfam.states.keySet
      var AmFiltered = Set.empty[List[Sentence]]
      val Amit = Am.iterator
      val progressor = Progressor("buildDQ", Am.size, 1)
      while (Amit.hasNext) {
        //progressor.tick
        val a = Amit.next()
        var existsToq = false
        breakable {
          val statesit = statesIds.iterator
          while (statesit.hasNext) {
            val id = statesit.next()
            if (sdfam.getDeltaOnSentence(id, a) == q) {
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

  private def buildGQsFromGraph(sdfam: SDFAMutant): Map[Int, Set[Int]] = {
    val statesIds = sdfam.states.keySet
    val gqs = statesIds.map(id => (id -> buildGQFromGraph(sdfam, id))).toMap
    gqs
  }

  private def buildGQFromGraph(
                                sdfam: SDFAMutant,
                                q: Int
                              ): Set[Int] = sdfam.buildGQ(q)

  // ∆−1 (q) = {p ∈ Q, ∃a ∈ A, δ(p, a) = q}
  private def buildGQ(
                       sdfam: SDFAMutant,
                       q: Int
                     ): Set[Int] = {
    val statesIds = sdfam.states.keySet
    val sentences = sdfam.sentences
    var statesFiltered = Set.empty[Int]
    val statesit = statesIds.iterator
    val progressor = Progressor("buildGQ", statesIds.size, 1)
    while (statesit.hasNext) {
      //progressor.tick
      val p = statesit.next()
      var existsToq = false
      breakable {
        val sentencesit = sentences.iterator
        while (sentencesit.hasNext) {
          val a = sentencesit.next()
          if (sdfam.getDeltaOnSentence(p, a) == q) {
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

  //not efficient, use version with graph
  private def buildDQs(
                        sdfam: SDFAMutant,
                        Am: Set[List[Sentence]]
                      ): Map[Int, Set[List[Sentence]]] = {
    val statesIds = sdfam.states.keySet
    //val dqs = statesIds.map(id => (id -> buildDQ(sdfam,Am,id))).toMap
    var dqs: Map[Int, Set[List[Sentence]]] = Map.empty
    val si = statesIds.iterator
    val progressor = Progressor("buildDQs", statesIds.size, 1)
    while (si.hasNext) {
      //progressor.tick
      val id = si.next()
      dqs = dqs + (id -> buildDQ(sdfam, Am, id))
    }
    dqs
  }

  //not efficient, use version with graph
  private def buildGQs(sdfam: SDFAMutant): Map[Int, Set[Int]] = {
    val statesIds = sdfam.states.keySet
    //val gqs = statesIds.map(id => (id -> buildGQ(sdfam,id))).toMap
    var gqs: Map[Int, Set[Int]] = Map.empty
    val si = statesIds.iterator
    val progressor = Progressor("buildGQs", statesIds.size, 1)
    while (si.hasNext) {
      //progressor.tick
      val id = si.next()
      gqs = gqs + (id -> buildGQ(sdfam, id))
    }
    gqs
  }

}

