package fsm.classical.fa.dfa

import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable
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
  * Require:A= (A,Q,s,F,δ) is a (m−1)-unambiguous DFA that recognize W
  *
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
  * @param dfa The DFA to be disambiguated.
  * @param m The disambiguation order.
  */
class Disambiguator private[dfa] (
                                   dfa: DFA,
                                   m: Int
                                 ) extends LazyLogging {
  require(m > 0)

  def disambiguate(): DFA = {
    var dfai = dfa
    var dqi = mutable.Map.empty[Int, mutable.Set[List[String]]]
    logger.debug("Disambiguating DFA...")
    for (i <- 1 to m) {
      logger.debug("Disambiguating up to " + i + "...")
      val res = disambiguateK(dfai, i)
      logger.debug("done with " + i + ".")
      dfai = res._1
      dqi = res._2
    }
    dfai.setOrderLabel(m, dqi)
    logger.debug("Disambiguation complete.")
    dfai
  }

  // Requires dfaMminus1 is (thism-1)-unambiguous
  private def disambiguateK(
                             dfaMminus1: DFA,
                             thism: Int
                           ): (DFA, mutable.Map[Int, mutable.Set[List[String]]]) = {
    require(thism > 0)
    require(isMUnambiguous(dfaMminus1, thism - 1))
    // Q0=Q, ∀q∈Q, Dq=δ−m(q) and Gq= ∆−1(q)
    val q0 = dfaMminus1.getStateKeys
    val Am: mutable.Set[List[String]] = buildAm(dfaMminus1, thism)
    val Dq: mutable.Map[Int, mutable.Set[List[String]]] = buildDQs(dfaMminus1, Am)
    val Gq: mutable.Map[Int, mutable.Set[Int]] = buildGQs(dfaMminus1)
    val A = dfaMminus1.getInputSymbols

    val q0Size = q0.size
    var counter = 0
    logger.debug("Scanning " + q0Size + " states...")
    for (qk <- q0) { // for all q∈Q0 do
      counter += 1
      logger.debug("Disambiguating up to " + thism + "@" + ((counter / q0Size.toDouble) * 100).toInt + "%")
      //println("Dis of state: " + qk)
      val thisDq = Dq(qk)
      val thisGq = Gq(qk)
      while (thisDq.size > 1) { // while |Dq|>1 do
        // take a=a1. . . am∈Dq
        val a = thisDq.head
        // add a new state qa to Q
        val qai = dfaMminus1.getMaxId + 1
        val qa = new DFAState(qai)
        qa.setOutput(dfaMminus1.getOutput(qk)) //if q∈F then add qa to F
        qa.setAsDuplicateOf(qk)
        // define Dqa ={a} and Gqa=∅
        val dqa = mutable.Set(a)
        val gca: mutable.Set[Int] = mutable.Set.empty
        // for all b∈A do
        for (b <- A) {
          // δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          // Not correct. Does not take into account loop states which are m-unambiguous.
          // Should be
          //  if q is loop state on b and a = b^m (i.e., a1=b,...,am=b) then
          //    δ(qa, b) = qa and add qa to Gqa
          //  else
          //    δ(qa, b) = δ(q, b) and add qa to Gδ(q,b)
          val dqb = selectNextState(dfaMminus1, qk, qai, a, b)
          qa.addDelta(b, dqb)
          if (dqb == qai) {
            gca += qai
          } else {
            Gq(dqb) += qai
          }
        }
        dfaMminus1.addState(qai, qa)
        // for all p∈Gq
        for (pi <- thisGq) {
          // if δ(p, am) = q and δ−(m−1)(p) =a1. . . am−1 (empty condition if m = 1) then
          val am = a.last
          if (dfaMminus1.delta(pi, am) == qk) {
            var tmp = true
            tmp = dm1Equalsa1m1(dfaMminus1, a, thism, pi)
            // δ(p, am) = qa and add p to Gqa
            if (tmp) {
              val p = dfaMminus1.getStates(pi)
              p.setDelta(am, qai)
              gca += pi
            }
          }
        }
        // for all p∈Gq
        for (pi <- thisGq) {
          // if q /∈ δ(p,A) then
          val dpA = dfaMminus1.delta(pi, A)
          if (!dpA.contains(qk)) {
            // remove q from Gq
            // "Remove q" is probably a mistake/typo. Should be "remove p"
            // remove p from Gq
            thisGq.remove(pi)
          }
        }
        // remove a from Dq
        thisDq.remove(a)

        Dq += (qai -> dqa)
        Gq += (qai -> gca)
      }
    }
    (dfaMminus1, Dq)
  }

  private def dm1Equalsa1m1(
                             thisdfa: DFA,
                             a: List[String],
                             thism: Int,
                             p: Int
                           ): Boolean = {
    require(thism > 0)
    if (thism == 1) return true
    val a1am1 = mutable.Set(a.init)
    val dm1 = dmOfState(thisdfa, p, thism - 1)
    dm1 == a1am1
  }

  private def selectNextState(
                               thisdfa: DFA,
                               q: Int,
                               qa: Int,
                               a: List[String],
                               b: String
                             ): Int = {
    var ns = 0
    if (areListElementsAll(b, a) & thisdfa.isLoopStateOn(q, b)) ns = qa
    else ns = thisdfa.delta(q, b)
    ns
  }

  private def areListElementsAll(
                                  b: String,
                                  a: List[String]
                                ): Boolean = a.forall(el => el == b)

  private def dmOfState(
                         thisdfa: DFA,
                         state: Int,
                         thism: Int
                       ): mutable.Set[List[String]] = {
    val Am = buildAm(thisdfa, thism)
    val dfaStates = thisdfa.getStates
    val dm = mutable.Set.empty[List[String]]
    for (a <- Am) {
      breakable {
        for ((kp, vp) <- dfaStates) {
          if (thisdfa.delta(kp, a) == state) {
            dm += a
            break
          }
        }
      }
    }
    dm
  }

  private[dfa] def isMUnambiguous(
                                   thisdfa: DFA,
                                   thism: Int
                                 ): Boolean = {
    require(thism >= 0)
    var result = true
    if (thism > 0) {
      val Am: mutable.Set[List[String]] = buildAm(thisdfa, thism)
      val Dq: mutable.Map[Int, mutable.Set[List[String]]] = buildDQs(thisdfa, Am)
      breakable {
        for ((dqk, dqv) <- Dq) {
          // condition should be size!=1, but initial state may result without any incoming edges
          if ((dqv.size > 1)) {
            //println("DFA: " + thisdfa.printDFA())
            //println("Am: " + Am)
            //println("Dq: " + Dq)
            logger.warn("Found non-singleton Dq:\n" + dqk + "\n" + dqv)
            //println("Found non-singleton Dq:\n" + dqk + "\n" + dqv)
            result = false
            break
          }
        }
      }
    }
    result
  }

  private def buildAm(
                       thisdfa: DFA,
                       thism: Int
                     ): mutable.Set[List[String]] = {
    val symbols = thisdfa.getInputSymbols
    var am = mutable.Set.empty[List[String]]
    am = am + List.empty
    for (i <- 1 to thism) {
      val newam = mutable.Set.empty[List[String]]
      for (a <- am) {
        for (s <- symbols) {
          val newa = a ::: List(s)
          newam += newa
        }
        //am -= a
      }
      am = newam
    }
    am
  }

  private def buildDQs(
                        thisdfa: DFA,
                        Am: mutable.Set[List[String]]
                      ): mutable.Map[Int, mutable.Set[List[String]]] = {
    val dqs = mutable.Map.empty[Int, mutable.Set[List[String]]]
    val dfaStates = thisdfa.getStates
    for ((k, v) <- dfaStates) {
      var newdq = mutable.Set.empty[List[String]]
      for (a <- Am) {
        breakable {
          for ((kp, vp) <- dfaStates) {
            if (thisdfa.delta(kp, a) == k) {
              newdq += a
              break
            }
          }
        }
      }
      dqs += (k -> newdq)
    }
    dqs
  }

  private def buildGQs(thisdfa: DFA): mutable.Map[Int, mutable.Set[Int]] = {
    val gqs = mutable.Map[Int, mutable.Set[Int]]()
    val dfaStates = thisdfa.getStates
    val symbols = thisdfa.getInputSymbols
    for ((k, v) <- dfaStates) {
      var newgq = mutable.Set[Int]()
      for ((kp, vp) <- dfaStates) {
        breakable {
          for (s <- symbols) {
            if (thisdfa.delta(kp, s) == k) {
              newgq += kp
              break
            }
          }
        }
      }
      gqs += (k -> newgq)
    }
    gqs
  }

}
