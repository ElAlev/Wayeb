package fsm.symbolic

import fsm.symbolic.logic.{EpsilonSentence, Predicate, Sentence}
import fsm.symbolic.sfa.snfa.{SNFA, SNFAState}
import fsm.symbolic.sfa.{SFAGuard, SFATransition}
import fsm.symbolic.sra.{SRAGuard, SRATransition}
import fsm.symbolic.sra.nsra.{NSRA, NSRAState}

/**
 * Class that eliminates all epsilon transitions from a Finite Automaton. After creating an Eliminator object, we call
 * fsm.symbolic.sfa.snfa.Eliminator#eliminate(scala.collection.immutable.Set) to construct the equivalent SNFA without
 * epsilon transitions.
 * The algorithm is essentially the same as that for classical automata.
 * See Section 2.5.5, Eliminating epsilon-Transitions in
 * @book{DBLP:books/daglib/0016921,
 *       author    = {John E. Hopcroft and
 *       Rajeev Motwani and
 *       Jeffrey D. Ullman},
 *       title     = {Introduction to automata theory, languages, and computation, 3rd Edition},
 *       series    = {Pearson international edition},
 *       publisher = {Addison-Wesley},
 *       year      = {2007}
 * }
 *
 *
 *
 * @param fa The original FA, possibly containing epsilon transitions.
 */
class Eliminator private[symbolic] (fa: Automaton) {
  val nonEpsilonSentences = fa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence])

  /**
   * Class used to track which states we have checked during elimination.
   */
  private class Tracker() {
    private var statesSeen: Set[Int] = Set[Int]()
    private var statesToSee: Set[Int] = Set[Int]()

    private var id2Set: Map[Int, Set[Int]] = Map[Int, Set[Int]]()
    private var set2Id: Map[Set[Int], Int] = Map[Set[Int], Int]()

    def addSeenState(sd: Int): Unit = {
      require(statesToSee.contains(sd))
      statesSeen = statesSeen + sd
      statesToSee = statesToSee - sd
    }

    def addStateToSee(s: Set[Int]): Int = {
      if (isStateSeen(s)) set2Id(s)
      else if (isStateToSee(s)) set2Id(s)
      else {
        val newId = getNewStateId
        statesToSee = statesToSee + newId
        id2Set += (newId -> s)
        set2Id += (s -> newId)
        newId
      }
    }

    def isStateSeen(s: Set[Int]): Boolean = {
      if (set2Id.contains(s)) statesSeen(set2Id(s))
      else false
    }

    def isStateToSee(s: Set[Int]): Boolean = {
      if (set2Id.contains(s)) statesToSee(set2Id(s))
      else false
    }

    def getNewStateId: Int = statesSeen.size + statesToSee.size

    def hasStatesToSee: Boolean = statesToSee.nonEmpty

    def getStatesToSee: Map[Int, Set[Int]] = {
      var statesToSeeMap = Map[Int, Set[Int]]()
      for (s <- statesToSee) {
        statesToSeeMap += (s -> id2Set(s))
      }
      statesToSeeMap
    }
  }

  def eliminate: Automaton = {
    var states = Set[Int]()
    var transitions = List[Transition]()
    var finals = Set[Int]()
    var dead = -1

    val tracker = new Tracker()
    val starte: Int = fa.start
    val startd = fa.enclose(starte)
    tracker.addStateToSee(startd)

    while (tracker.hasStatesToSee) {
      val states2See = tracker.getStatesToSee
      for ((newId, stateSet) <- states2See) {
        states = states + newId
        tracker.addSeenState(newId)
        val successors = buildSuccessorsForState(stateSet)
        for ((pred, nextSet) <- successors) {
          val nextId = tracker.addStateToSee(nextSet)
          val newTransition = fa match {
            case _: SNFA => SFATransition(newId, nextId, SFAGuard(pred))
            case _: NSRA => SRATransition(newId, nextId, SRAGuard(pred))

          }
          transitions = newTransition :: transitions
        }
        if (stateSet.intersect(fa.finals).nonEmpty) finals = finals + newId
        if (stateSet.isEmpty) dead = newId
      }

    }
    fa match {
      case _: SNFA => {
        val elsnfaStates = states.map(s => (s,SNFAState(s))).toMap
        val snfaTransitions = transitions.map(t => SFATransition(t.source, t.target, SFAGuard(t.guard.sentence)))
        val elsnfa = SNFA(elsnfaStates, snfaTransitions, 0, finals)
        elsnfa.setStateAsDead(dead)
        elsnfa
      }
      case _: NSRA => {
        val elnsraStates = states.map(s => (s,NSRAState(s))).toMap
        val nsraTransitions = transitions.map(t => SRATransition(t.source, t.target, SRAGuard(t.guard.sentence)))
        val elnsra = NSRA(elnsraStates, nsraTransitions, 0, finals)
        elnsra
      }
    }



  }

  private def buildSuccessorsForState(stateSet: Set[Int]): Map[Sentence, Set[Int]] = {
    val succ = nonEpsilonSentences.map(s => (s -> fa.enclose(fa.getSuccessors(stateSet, s, Set.empty[Set[Predicate]])))).toMap
    succ
  }

}
