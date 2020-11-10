package fsm.classical.fa.nfa

//import utils.SetUtils
import scala.collection.mutable

/**
  * Class that eliminates all epsilon transitions from a NFA. After creating an Eliminator object, we call
  * fsm.classical.nfa.Eliminator#eliminate() to construct the equivalent NFA without epsilon transitions.
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
  * @param nfa The originial NFA, possibly having epsilon transitions.
  */
class Eliminator private[nfa] (nfa: NFA) {

  def eliminate(): NFA = buildElimNFA(nfa)

  /**
    * Class used to track which states we have checked during elimination.
    */
  private class Tracker() {
    private val statesSeen: mutable.Set[Int] = mutable.Set[Int]()
    val statesToSee: mutable.Set[Int] = mutable.Set[Int]()

    private val id2Set: mutable.Map[Int, mutable.Set[Int]] = mutable.Map[Int, mutable.Set[Int]]()
    private val set2Id: mutable.Map[mutable.Set[Int], Int] = mutable.Map[mutable.Set[Int], Int]()

    def addSeenState(sd: Int): Unit = {
      require(statesToSee.contains(sd))
      statesSeen.add(sd)
      statesToSee.remove(sd)
    }

    def addStateToSee(s: mutable.Set[Int]): Int = {
      if (isStateSeen(s)) set2Id(s)
      else if (isStateToSee(s)) set2Id(s)
      else {
        val newId = getNewStateId
        statesToSee.add(newId)
        id2Set += (newId -> s)
        set2Id += (s -> newId)
        newId
      }
    }

    def isStateSeen(s: Int): Boolean = statesSeen.contains(s)

    def isStateSeen(s: mutable.Set[Int]): Boolean = {
      if (set2Id.contains(s)) {
        if (statesSeen(set2Id(s))) true
        else false
      } else false
    }

    def isStateToSee(s: mutable.Set[Int]): Boolean = {
      if (set2Id.contains(s)) {
        if (statesToSee(set2Id(s))) true
        else false
      } else false
    }

    def getNumberOfStatesSeen: Int = statesSeen.size

    def getNewStateId: Int = statesSeen.size + statesToSee.size

    def hasStatesToSee: Boolean = statesToSee.nonEmpty

    def getStatesToSee: mutable.Map[Int, mutable.Set[Int]] = {
      val statesToSeeMap = mutable.Map[Int, mutable.Set[Int]]()
      for (s <- statesToSee) {
        statesToSeeMap += (s -> id2Set(s))
      }
      statesToSeeMap
    }
  }

  private def buildElimNFA(epsilonNfa: NFA): NFA = {
    val elimNfa = new NFA
    val inputSymbols = epsilonNfa.getInputSymbols
    inputSymbols.remove("$")

    val tracker = new Tracker()
    val starte = epsilonNfa.getStartId
    val startd = epsilonNfa.enclose(starte)
    tracker.addStateToSee(startd)

    while (tracker.hasStatesToSee) {
      val states2See = tracker.getStatesToSee
      for ((newId, stateSet) <- states2See) {
        val newNFAState = new NFAState(newId)
        tracker.addSeenState(newId)
        val successors = buildSuccessorsForState(stateSet, inputSymbols, epsilonNfa)
        for ((a, nextSet) <- successors) {
          val nextId = tracker.addStateToSee(nextSet)
          newNFAState.addDelta(a, nextId)
        }
        elimNfa.addState(newId, newNFAState)
        if (stateSet.intersect(epsilonNfa.getAcceptingId).nonEmpty) elimNfa.setStateAsAccepting(newId)
        if (stateSet.isEmpty) elimNfa.setStateAsDead(newId)
      }
    }
    elimNfa.setStateAsStart(0)
    elimNfa.setInputSymbols(inputSymbols)
    elimNfa
  }

  private def buildSuccessorsForState(
                                       stateSet: mutable.Set[Int],
                                       inputSymbols: mutable.Set[String],
                                       epsilonNfa: NFA
                                     ): mutable.Map[String, mutable.Set[Int]] = {
    val successors = mutable.Map.empty[String, mutable.Set[Int]]
    for (a <- inputSymbols) {
      var r = mutable.Set[Int]()
      for (p <- stateSet) {
        r = r ++ epsilonNfa.getDelta(p, a)
      }
      val dd = epsilonNfa.enclose(r.toSet)
      successors += (a -> dd)
    }
    successors
  }

  /*
  def eliminate(): NFA = {
    val elNfa = new NFA

    val id2Set = mutable.Map[Int, mutable.Set[Int]]()
    val set2Id = mutable.Map[mutable.Set[Int], Int]()
    val qd = buildQD()
    val startd = buildStart(qd)
    val acceptingd = buildAccepting(qd)

    var newStateId = -1
    for (s <- qd) {
      newStateId += 1
      id2Set += (newStateId -> s)
      set2Id += (s -> newStateId)
      val newState = new NFAState(newStateId)
      elNfa.addState(newStateId, newState)
      if (s == startd) elNfa.setStateAsStart(newStateId)
      if (acceptingd.contains(s)) elNfa.setStateAsAccepting(newStateId)
    }

    val inputSymbols = nfa.getInputSymbols
    inputSymbols.remove("$")
    elNfa.setInputSymbols(inputSymbols)

    // now create a dead state
    val deadStateId = newStateId + 1
    val deadState = new NFAState(deadStateId)
    elNfa.addState(deadStateId, deadState)
    elNfa.setStateAsDead(deadStateId)
    for (a <- inputSymbols) elNfa.addDelta(deadStateId, a, deadStateId)

    //newStateId = -1
    for (sid <- 0 to newStateId) {
      val s = id2Set(sid)
      for (a <- inputSymbols) {
        val r = nfa.getDelta(s, a)
        val nextStatesSet = nfa.enclose(r.toSet)
        var nextStateId = -1
        if (nextStatesSet.isEmpty) nextStateId = deadStateId
        else nextStateId = set2Id(nextStatesSet)
        elNfa.addDelta(sid, a, nextStateId)
      }
    }

    elNfa
  }

  private def buildStart(qd: mutable.Set[mutable.Set[Int]]): mutable.Set[Int] = {
    val starte = nfa.getStartId
    val startd = nfa.enclose(starte)
    if (!qd.contains(startd)) throw new IllegalArgumentException("Start of DFA is not in set of DFA states")
    startd
  }

  private def buildAccepting(qd: mutable.Set[mutable.Set[Int]]): mutable.Set[mutable.Set[Int]] = {
    val acceptinge = nfa.getAcceptingId.head
    val acceptingd = mutable.Set[mutable.Set[Int]]()
    for (q <- qd) {
      if (q.contains(acceptinge)) acceptingd.add(q)
    }
    acceptingd
  }

  private def buildQD(): mutable.Set[mutable.Set[Int]] = {
    val qe = nfa.getAllStates.keys.toSet
    val qeps = SetUtils.power[Int](qe)
    val qd = mutable.Set[mutable.Set[Int]]()
    for (qes <- qeps) {
      if (qes.nonEmpty) {
        val qen = nfa.enclose(qes)
        if (qes == qen) qd.add(qen)
      }
    }
    qd
  }

  */

}
