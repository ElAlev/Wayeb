package fsm.symbolic.sra

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.StateMapper
import fsm.symbolic.logic.{LogicUtils, Predicate, Sentence}
import fsm.symbolic.sra.dsra.{DSRA, DSRAState}
import fsm.symbolic.sra.nsra.{NSRA, NSRAUtils}
import scala.annotation.tailrec

object SRAUtils extends LazyLogging {

  /**
   * Constructs a deterministic SRA equivalent to a windowed, non-deterministic SRA.
   * Any epsilon transitions are first removed from the windowed NSRA.
   * Then the SRA is unrolled and then the unrolled SRA is determinized.
   *
   * @param nsra The given non-deterministic SRA.
   * @param window The given window.
   * @return The deterministic SRA.
   */
  def determinize(
                   nsra: NSRA,
                   window: Int
                 ): DSRA = {
    require(window > 1)
    val elimNSRA = NSRAUtils.eliminateEpsilon(nsra)
    val unrolledNSRA = NSRAUtils.unroll(elimNSRA, window)
    val dsra = SRAUtils.determinizeUnrolled(unrolledNSRA)
    dsra
  }

  /**
   * Determinizes an unrolled SRA. The given SRA must have no epsilon transitions and must have been unrolled.
   * The algorithm works incrementally, by maintaining a set of front states and expanding them gradually to include new
   * states for the deterministic SRA.
   *
   * @param nsra The given unrolled SRA.
   * @return The deterministic SRA.
   */
  def determinizeUnrolled(nsra: NSRA): DSRA = {
    require(!nsra.hasEpsilon, "NSRA must have no epsilon transitions before determinization")
    val stateMapping = StateMapper(nsra)
    val frontStates = Set(stateMapping.getStartId)
    val statesNtransitions = expand(frontStates, Set(stateMapping.getStartId), List.empty[SRATransition], nsra, stateMapping)
    val states = statesNtransitions._1.map(s => (s, DSRAState(s))).toMap
    val transitions = statesNtransitions._2
    val finals = states.keySet.filter(id => stateMapping.getAsSet(id).intersect(nsra.finals).nonEmpty)
    val start = stateMapping.getStartId
    val dsra = DSRA(states, transitions, start, finals)
    dsra
  }

  /**
   * Recursive function which creates incrementally all deterministic states and transitions starting from a given set
   * of already existing states.
   *
   * @param frontStates The states that need to be expanded at this step. For the first call, it should just contain
   *                    only the start state of the deterministic automaton.
   * @param states The states created thus far.
   * @param transitions The transitions created thus far.
   * @param nsra The non-deterministic SRA.
   * @param smi The state mapper. For the first call, it should contain the start state of the deterministic SRA and its
   *            corresponding set of non-deterministic states.
   * @return The set of deterministic states and the list of deterministic transitions.
   */
  @tailrec
  private def expand(
                      frontStates: Set[Int],
                      states: Set[Int],
                      transitions: List[SRATransition],
                      nsra: NSRA,
                      smi: StateMapper
                    ): (Set[Int], List[SRATransition]) = {
    val fsl = frontStates.toList
    fsl match {
      case Nil => (states, transitions)
      case head :: tail => {
        val st = getNewStatesAndTransitionsFromSource(nsra, head, smi)
        val newStatesFromSource = st._1
        val newTransitionsFromSource = st._2
        val newFrontStates = tail.toSet ++ newStatesFromSource.diff(states)
        val newStates = states ++ newStatesFromSource
        val newTransitions = transitions ::: newTransitionsFromSource
        expand(newFrontStates, newStates, newTransitions, nsra, smi)
      }
    }
  }

  /**
   * For a given non-deterministic SRA and a given state of the deterministic SRA, finds a set of deterministic
   * target states and a list of the corresponding transitions.
   *
   * @param nsra The given non-deterministic SRA.
   * @param sourceDet The state of the deterministic SRA.
   * @param smi The state mapper which assigns states of the deterministic SRA to sets of states of the NSRA.
   * @return A set of deterministic target states and a list of the corresponding transitions
   */
  private def getNewStatesAndTransitionsFromSource(
                                                    nsra: NSRA,
                                                    sourceDet: Int,
                                                    smi: StateMapper
                                                  ): (Set[Int], List[SRATransition]) = {
    // first find the set of non-deterministic states corresponding to the deterministic state
    val sourceSet = smi.getAsSet(sourceDet)
    // construct the minterms and their write registers from these states
    val minTermsWithRegisters = buildMinTermsFromSource(nsra, sourceSet)
    // now create all the deterministic states and transitions from the minterms
    val st = minTermsWithRegisters.map(s => getNewStateAndTransitionFromSource(nsra, sourceDet, sourceSet,  smi, s._1, s._2)).toList
    val newStates = st.map(s => s._1).toSet
    val newTransitions = st.map(s => s._2)
    (newStates, newTransitions)
  }

  /**
   * From a given deterministic state abd a given sentence/minterm, find/create the target deterministic state and
   * create the corresponding transition.
   *
   * @param nsra The given NSRA.
   * @param source The deterministic state.
   * @param sourceSet The set of non-deterministic states corresponding to the deterministic state.
   * @param smi The state mapper.
   * @param sentence The minterm.
   * @param writeRegisters The write registers of the minterm.
   * @return The id of the deterministic target state and the deterministic transition.
   */
  private def getNewStateAndTransitionFromSource(
                                                  nsra: NSRA,
                                                  source: Int,
                                                  sourceSet: Set[Int],
                                                  smi: StateMapper,
                                                  sentence: Sentence,
                                                  writeRegisters: Set[String]
                                                ): (Int, SRATransition) = {
    val exclusives = Set.empty[Set[Predicate]]
    // find all the non-deterministic states we can reach for the current deterministic state (from its corresponding
    // set of non-deterministic states)
    val successors = nsra.getSuccessors(sourceSet, sentence, exclusives)
    // add the new state to the state mapper
    val target = smi.addNewDetState(successors)
    // create the new transition now that we have everything we need
    val transition = SRATransition(source, target, SRAGuard(sentence), writeRegisters)
    (target, transition)
  }

  /**
   * Constructs the minterms from the outgoing transitions of a set of non-deterministic states of a given SRA.
   *
   * @param nsra The given NSRA.
   * @param sourceSet The set of non-deterministic states for which we need to construct the minterms.
   * @return A set of minterms. Each minterm is accompanied by a set of write registers, i.e., the registers to which it
   *         should write if triggered.
   */
  private def buildMinTermsFromSource(
                                     nsra: NSRA,
                                     sourceSet: Set[Int]
                                     ): Set[(Sentence, Set[String])] = {
    // find all the outgoing transitions from the set of states
    val transitionsFromSources = nsra.transitions.filter(t => sourceSet.contains(t.source))
    // keep the sentences of these transitions
    val sentencesFromSources = transitionsFromSources.map(t => t.guard.sentence).filter(s => !s.isTrue)
    // now build the minterms (all possible conjuncts of positive and negative literals)
    val minTerms = LogicUtils.buildMinTerms(sentencesFromSources.toSet, Set.empty)
    // for each minterm, find its write registers
    val minTermsWithRegisters = minTerms.map(mt => (mt, findWriteRegistersForMinterm(transitionsFromSources, mt)))
    minTermsWithRegisters
  }

  /**
   * Finds the write registers of a minterm from a list of transitions.
   *
   * @param relevantTransitions The list of transitions.
   * @param minTerm The minterm.
   * @return The set of write registers as a set of strings.
   */
  private def findWriteRegistersForMinterm(
                                            relevantTransitions: List[SRATransition],
                                            minTerm: Sentence
                                          ): Set[String] = {
    // from all given transitions, keep only those that would be triggered if the minterm is triggered
    val entailedTransitions = relevantTransitions.filter(t => minTerm.entails(t.guard.sentence))
    // now gather all the registers from the retained transitions
    val entailedRegisters = entailedTransitions.flatMap(t => t.writeRegisters).toSet
    entailedRegisters
  }
}
