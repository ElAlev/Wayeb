package fsm.symbolic.sfa.snfa

import fsm.symbolic.Constants
import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.logic.{EpsilonSentence, Sentence}
import fsm.symbolic.sfa.{SFAGuard, SFATransition}

/**
  * Class that eliminates all epsilon transitions from a SNFA. After creating an Eliminator object, we call
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
  * @param snfa The original SNFA, possibly containing epsilon transitions.
  */
class Eliminator private[snfa] (snfa: SNFA) {
  val nonEpsilonSentences: Set[Sentence] = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence])

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

    def getNewStateId: Int = statesSeen.size + statesToSee.size + 1

    def hasStatesToSee: Boolean = statesToSee.nonEmpty

    def getStatesToSee: Map[Int, Set[Int]] = {
      var statesToSeeMap = Map[Int, Set[Int]]()
      for (s <- statesToSee) {
        statesToSeeMap += (s -> id2Set(s))
      }
      statesToSeeMap
    }
  }

  def eliminate: SNFA = {
    var states = Map[Int, SNFAState]()
    var transitions = List[SFATransition]()
    var finals = Set[Int]()
    var dead = Constants.deadStateIdConstant //-1

    val tracker = new Tracker()
    val starte = snfa.start
    val startd = snfa.enclose(starte)
    tracker.addStateToSee(startd)

    while (tracker.hasStatesToSee) {
      val states2See = tracker.getStatesToSee
      for ((newId, stateSet) <- states2See) {
        val newSNFAState = SNFAState(newId)
        states = states + (newId -> newSNFAState)
        tracker.addSeenState(newId)
        val successors = buildSuccessorsForState(stateSet)
        for ((pred, (nextSet, output)) <- successors) {
          val nextId = tracker.addStateToSee(nextSet)
          val newTransition = SFATransition(newId, nextId, SFAGuard(pred), output)
          transitions = newTransition :: transitions
        }
        if (stateSet.intersect(snfa.finals).nonEmpty) finals = finals + newId
        if (stateSet.isEmpty) dead = newId
      }

    }
    val elsnfa = SNFA(states, transitions, 1, finals)//shiftStateIds(SNFA(states, transitions, 1, finals))
    elsnfa.setStateAsDead(dead)
    elsnfa
  }

  private def shiftStateIds(snfa: SNFA): SNFA = {
    val newStates = snfa.states.map(s => (s._1+1, SNFAState(s._1+1)))
    val newTransitions = snfa.transitions.map(t => SFATransition(t.source + 1, t.target +1, t.guard.asInstanceOf[SFAGuard], t.output))
    val newStart = snfa.start + 1
    val newFinals = snfa.finals.map(f => f + 1)
    val shiftedSNFA = SNFA(newStates, newTransitions, newStart, newFinals)
    shiftedSNFA
  }

  private def buildSuccessorsForState(stateSet: Set[Int]): Map[Sentence, (Set[Int], TransitionOutput)] = {
    val relevantTransitions = snfa.transitions.filter(t => stateSet.contains(t.source) & !t.guard.sentence.isInstanceOf[EpsilonSentence])
    val sentences1 = relevantTransitions.map(t => (t.guard.sentence, t.output)).toSet
    val succ1 = sentences1.map(s => {
      val states = relevantTransitions.filter(t => t.guard.isSentence(s._1)).flatMap(x => snfa.enclose(x.target)).toSet
      (s._1, (states, s._2))
    }).toMap

    //val sentences = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence])
    //val succ = sentences.map(s => (s -> snfa.enclose(snfa.getSuccessors(stateSet, s, Set.empty[Set[Predicate]])))).toMap

    succ1
  }

}
