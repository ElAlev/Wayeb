package fsm.symbolic.sfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.Constants.deadId
import fsm.symbolic.sfa.logic.{EpsilonSentence, LogicUtils, Predicate, Sentence}
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAState}
import fsm.symbolic.sfa.snfa.SNFA

class Determinizer {

}

object Determinizer extends LazyLogging {
  /**
    * Determinizes a SNFA, while also taking into account declarations for exclusives and extras.
    *
    * @param snfa The SNFA to be determinized.
    * @param exclusives The set of exclusives.
    * @param extras The set of extras.
    * @param idg The id generator.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinize(
                   snfa: SNFA,
                   exclusives: Set[Set[Predicate]],
                   extras: Set[Sentence],
                   idg: IdGenerator
                 ): SDFA = {
    // first extract all sentences, except for epsilon and true
    val sentences = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence] & !s.isTrue)
    // add any extras
    val sentencesWithExtras = sentences ++ extras
    logger.debug("Building min-terms")
    var t1 = System.nanoTime()
    // based on all sentences and extras, along with the exclusives, build the minterms
    val minTerms = LogicUtils.buildMinTerms(sentencesWithExtras, exclusives)
    var t2 = System.nanoTime()
    logger.debug(minTerms.size + " min-terms built in " + (t2 - t1) / 1000000.0 + " ms.")
    t1 = System.nanoTime()
    logger.debug("Creating powerset")
    // let's create the powerset of SNFA states and give them ids
    val stateMapping = new StateMapper(snfa, idg)
    val ids = stateMapping.getIds
    logger.debug("Creating transitions")
    // we now construct the transitions
    val normalTransitions = ids.map(id => getTransitionsFromSource(snfa, id, stateMapping, exclusives, minTerms)).
      toList.flatten
    logger.debug("Creating states")
    val normalStates = ids.map(id => (id, SDFAState(id))).toMap
    val hasDead = normalTransitions.exists(t => t.target == deadId)
    // if the transitions need a dead state, then we create such a state and add it
    val states = if (hasDead) normalStates + (deadId -> SDFAState(deadId)) else normalStates
    // similarly, of we have a dead state, we need to create self-loop transitions on the dead state with all minterms
    val transitions = //if (hasDead) Transition(deadId,deadId,Guard(SentenceConstructor.getNewTrueSentence))::normalTransitions
      if (hasDead) deadTransitions(minTerms) ::: normalTransitions
      else normalTransitions
    // the start SDFA state is the enclosure of the SNFA state
    val start = stateMapping.getAsId(snfa.enclose(snfa.start))
    // a SDFA state is final if one of its members is final in the SNFA
    val finals = ids.filter(id => stateMapping.getAsSet(id).intersect(snfa.finals).nonEmpty)
    val sdfa = SDFA(states, transitions, start, finals)
    logger.debug("Initial SDFA has " + sdfa.states.size + " states and " + sdfa.transitions.size + " transitions")
    logger.debug("Removing inaccessible states")
    // using the powerset method may result in the create of some inaccessible state which we need to remove
    val accessSdfa = removeInaccessible(sdfa)
    t2 = System.nanoTime()
    logger.debug("Final SDFA has " + accessSdfa.states.size + " states and " + accessSdfa.transitions.size + " transitions")
    //println(accessSdfa.toString)
    logger.debug("SDFA built in " + (t2 - t1) / 1000000.0 + " ms.")
    accessSdfa
  }

  /**
    * For every minterm, creates a self-loop transition on the dead state.
    *
    * @param minterms The minterms.
    * @return The self-loop transitions on the dead state.
    */
  private def deadTransitions(minterms: Set[Sentence]): List[Transition] =
    minterms.toList.map(s => Transition(deadId, deadId, Guard(s)))

  /**
    * Removes every inaccessible state, along with any transitions associated with inaccessible states.
    *
    * @param sdfa The original SDFA, possibly containing inaccessible states.
    * @return The SDFA without any inaccessible states.
    */
  private def removeInaccessible(sdfa: SDFA): SDFA = {
    val accesibleIds = sdfa.getAccessibleStateIDs
    val accessibleStates = sdfa.states.filter(state => accesibleIds.contains(state._1))

    val validTransitions = sdfa.transitions.filter(t => accesibleIds.contains(t.source) &
      accesibleIds.contains(t.target))
    val start = sdfa.start
    val finals = sdfa.finals.intersect(accesibleIds)
    SDFA(accessibleStates, validTransitions, start, finals)
  }

  /**
    * Creates all outgoing transitions for a given SDFA state.
    *
    * @param snfa The SNFA.
    * @param source The source SDFA state for the transitions.
    * @param sm The state mapper.
    * @param exclusives The exclusives.
    * @param sentences The sentences that will act as transition guards. One transition to be created for each sentence.
    * @return All outgoing SDFA transitions from source.
    */
  private def getTransitionsFromSource(
                                        snfa: SNFA,
                                        source: Int,
                                        sm: StateMapper,
                                        exclusives: Set[Set[Predicate]],
                                        sentences: Set[Sentence]
                                      ): List[Transition] = {
    // One transition for each sentence.
    sentences.map(s => getNewTransition(snfa, source, sm, exclusives, s)).toList
  }

  /**
    * Creates an outgoing transition from a given SDFA state with a given sentence.
    *
    * @param snfa The SNFA.
    * @param source The source SDFA state for the transition.
    * @param sm The state mapper.
    * @param exclusives The exclusives.
    * @param sentence The sentence that will act as transition guard.
    * @return The SDFA outgoing transition.
    */
  private def getNewTransition(
                                snfa: SNFA,
                                source: Int,
                                sm: StateMapper,
                                exclusives: Set[Set[Predicate]],
                                sentence: Sentence
                              ): Transition = {
    // find the source state as set of SNFA states
    val se = sm.getAsSet(source)
    // get all successor states with the given sentence
    val successors = snfa.getSuccessors(se, sentence, exclusives)
    // now enclose successors
    val enclosedSuccessors = snfa.enclose(successors)
    // if enclosure is empty, this means that there is no next state with the given sentence, so the transition should
    // go to the dead state
    val target = if (enclosedSuccessors.isEmpty) deadId else sm.getAsId(enclosedSuccessors)
    Transition(source, target, Guard(sentence))
  }

  /**
    * Helper class for determinization. Creates the powerset of SNFA states and assigns to every set a unique id.
    *
    * @param snfa The SNFA.
    * @param idg The id generator, in case some state ids have been reserved,
    *            e.g., in negation,
    *            see fsm.symbolic.sfa.snfa.SNFAUtils#makeSNFAComplete(fsm.symbolic.sfa.snfa.SNFA, fsm.symbolic.sfa.IdGenerator).
    */
  private class StateMapper(
                             snfa: SNFA,
                             idg: IdGenerator
                           ) {
    // these are the states' ids of the SNFA
    private val qe: Set[Int] = snfa.getStates
    // now create the SDFA states, as sets of SNFA states, by creating the powerset of qe
    //TODO: Accessible states are supposed to be those for which S=enclose(S) (Aho et al, 2.5.5)
    //      But does not seem to be the case here. Filtering keeps inaccessible. Check it.
    private val qd: Set[Set[Int]] = utils.SetUtils.power(qe).filter(q => snfa.enclose(q) == q)
    // create the unique ids for the SDFA states
    private val idsFromIdg = for (q <- qd) yield idg.getId
    // a mapping of SDFA states as sets to SDFA states as unique ids
    private val stateMapping: List[(Set[Int], Int)] = qd.toList.zip(idsFromIdg.toList)
    private val set2Id: Map[Set[Int], Int] = stateMapping.toMap

    val (sets, ids) = stateMapping.unzip

    // and an inverse mapping of SDFA states as unique ids to SDFA states as sets
    private val id2Set: Map[Int, Set[Int]] = ids.zip(sets).toMap

    /**
      * Returns the set of SNFA states corresponding to the id of the SDFA state.
      *
      * @param id The id of the SDFA state.
      * @return The corresponding set of SNFA states.
      */
    def getAsSet(id: Int): Set[Int] = {
      require(id2Set.contains(id))
      id2Set(id)
    }

    /**
      * Returns the id of the SDFA state corresponding to the set of SNFA states.
      *
      * @param set The set of SNFA states.
      * @return The corresponding id of the SDFA state.
      */
    def getAsId(set: Set[Int]): Int = {
      require(set2Id.contains(set))
      set2Id(set)
    }

    /**
      * Retrieves all unique SDFA state ids.
      *
      * @return All unique SDFA state ids
      */
    def getIds: Set[Int] = ids.toSet
  }
}
