package fsm.symbolic.sfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.StateMapper
import fsm.symbolic.logic.{EpsilonSentence, LogicUtils, Predicate, Sentence}
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAState}
import fsm.symbolic.sfa.snfa.SNFA
import fsm.symbolic.Constants.deadStateIdConstant

/**
  * An incremental way to determinize a SNFA without first creating the power-set of its states.
  */

class DeterminizerIncr {

}

object DeterminizerIncr extends LazyLogging {

  /**
    * An incremental way to determinize a SNFA without first creating the power-set of its states.
    * First we need to create the min-terms of all the predicates to be found in the pattern and the SNFA (don't care
    * about TRUE and epsilon predicates).
    * We then start from the start state of the SNFA, find all the epsilon-accessible states of it and set
    * this set as the start state of the SDFA. We then incrementally expand this start state by recursively finding all
    * its successors and then the successors of those successors etc.
    *
    * @param snfa The original SNFA.
    * @param exclusives Exclusives (if any).
    * @param extras Extras (if any)
    * @return The deterministic SDFA equivalent to the SNFA.
    */
  def determinize(
                   snfa: SNFA,
                   exclusives: Set[Set[Predicate]],
                   extras: Set[Sentence],
                   minTermMethod: String
                 ): SDFA = {
    val sentences = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence] & !s.isTrue)
    val sentencesWithExtras = sentences ++ extras
    logger.debug("Building min-terms")
    var t1 = System.nanoTime()
    val minTerms = LogicUtils.buildMinTerms(sentencesWithExtras, exclusives, minTermMethod)
    var t2 = System.nanoTime()
    logger.debug(minTerms.size + " min-terms built in " + (t2 - t1) / 1000000.0 + " ms.")
    t1 = System.nanoTime()
    val stateMapping = StateMapper(snfa) //new StateMapperI(snfa)
    logger.debug("Creating states and transitions for SDFA")
    val frontStates = Set(stateMapping.getStartId)
    val statesNtransitions = expand(frontStates, Set(stateMapping.getStartId), List.empty[SFATransition], snfa, stateMapping, exclusives, minTerms)
    val states = statesNtransitions._1.map(s => (s, SDFAState(s))).toMap
    val transitions = statesNtransitions._2
    val finals = states.keySet.filter(id => stateMapping.getAsSet(id).intersect(snfa.finals).nonEmpty)
    val start = stateMapping.getStartId
    val sdfa = SDFA(states, transitions, start, finals)
    t2 = System.nanoTime()
    logger.debug("SDFA has " + sdfa.states.size + " states and " + sdfa.transitions.size + " transitions")
    logger.debug("SDFA built in " + (t2 - t1) / 1000000.0 + " ms.")
    sdfa
  }

  /**
    * From a set of states of the SDFA (already created), we find (and create) all the states where we can go next,
    * given a set of sentences (essentially min-terms).
    *
    * @param frontStates The set of SDFA states which we need to check. These must not have already been checked. They
    *                    constitute a front during the incremental search.
    * @param states Accumulator for all states of the SDFA.
    * @param transitions  Accumulator for all transitions of the SDFA.
    * @param snfa The original SNFA.
    * @param smi The state mapper, see fsm.symbolic.sfa.DeterminizerIncr.StateMapperI.
    * @param exclusives Exclusives (if any).
    * @param sentences The sentences of the SDFA to be checked, essentially the min-terms.
    * @return
    */
  @scala.annotation.tailrec
  private def expand(
                      frontStates: Set[Int],
                      states: Set[Int],
                      transitions: List[SFATransition],
                      snfa: SNFA,
                      smi: StateMapper,
                      exclusives: Set[Set[Predicate]],
                      sentences: Set[Sentence]
                    ): (Set[Int], List[SFATransition]) = {
    val fsl = frontStates.toList
    fsl match {
      case Nil => (states, transitions)
      case head :: tail => {
        val st = getNewStatesAndTransitionsFromSource(snfa, head, smi, exclusives, sentences)
        val newStatesFromSource = st._1
        val newTransitionsFromSource = st._2
        val newFrontStates = tail.toSet ++ newStatesFromSource.diff(states)
        val newStates = states ++ newStatesFromSource
        val newTransitions = transitions ::: newTransitionsFromSource
        expand(newFrontStates, newStates, newTransitions, snfa, smi, exclusives, sentences)
      }
    }
  }

  /**
    * Same as fsm.symbolic.sfa.DeterminizerIncr#getNewStatesAndTransitionsFromSource(fsm.symbolic.sfa.snfa.SNFA, int, fsm.symbolic.sfa.DeterminizerIncr.StateMapperI, scala.collection.immutable.Set, fsm.symbolic.sfa.logic.Sentence)
    * but for multiple sentences.
    *
    * @param snfa The original SNFA.
    * @param source The source state of the SDFA (must have been already created).
    * @param smi The state mapper. Maps/tracks SDFA states to SNFA (sets of) states.
    * @param exclusives Exclusives (if any).
    * @param sentences The sentences that trigger the transitions from the source state.
    * @return A set of new states from source along with the transitions from source to new states.
    */
  private def getNewStatesAndTransitionsFromSource(
                                                    snfa: SNFA,
                                                    source: Int,
                                                    smi: StateMapper,
                                                    exclusives: Set[Set[Predicate]],
                                                    sentences: Set[Sentence]
                                                  ): (Set[Int], List[SFATransition]) = {
    val st = sentences.map(s => getNewStatesAndTransitionsFromSource(snfa, source, smi, exclusives, s)).toList
    val newStates = st.map(s => s._1).toSet
    val newTransitions = st.map(s => s._2)
    (newStates, newTransitions)
  }

  /**
    * For a state of the SDFA, we find its next state given a sentence (essentially a min-term).
    * First get the set of SNFA states corresponding to the source state of the SDFA.
    * Then get all the successor states in the SNFA, given the sentence.
    * And enclose these successor states.
    * The resulting set of states is the new state of the SDFA with a transition guarded by the sentence/min-term.
    *
    * @param snfa The original SNFA.
    * @param source The source state of the SDFA (must have been already created).
    * @param smi The state mapper. Maps/tracks SDFA states to SNFA (sets of) states.
    * @param exclusives Exclusives (if any).
    * @param sentence The sentence that triggers the transitions from the source state.
    * @return A new target state for the SDFA along with its transition.
    */
  private def getNewStatesAndTransitionsFromSource(
                                                    snfa: SNFA,
                                                    source: Int,
                                                    smi: StateMapper,
                                                    exclusives: Set[Set[Predicate]],
                                                    sentence: Sentence
                                                  ): (Int, SFATransition) = {
    val sourceSet = smi.getAsSet(source)
    val successors = snfa.getSuccessorsFromGraph(sourceSet, sentence, exclusives)
    val enclosedSuccessors = snfa.encloseFromGraph(successors)
    val target = smi.addNewDetState(enclosedSuccessors)
    val transition = SFATransition(source, target, SFAGuard(sentence))
    (target, transition)
  }

}
