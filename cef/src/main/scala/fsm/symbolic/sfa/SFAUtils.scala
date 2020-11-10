package fsm.symbolic.sfa

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sfa.logic._
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.symbolic.sfa.snfa.SNFA
import ui.ConfigUtils

object SFAUtils extends LazyLogging {

  /**
    * Determinizes a SNFA.
    *
    * @param snfa The SNFA to be determinized.
    * @param idg The id generator.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinize(
                   snfa: SNFA,
                   idg: IdGenerator
                 ): SDFA = {
    Determinizer.determinize(snfa, Set.empty, Set.empty, idg)
  }

  /**
    * Determinizes a SNFA.
    *
    * @param snfa The SNFA to be determinized.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinize(snfa: SNFA): SDFA = {
    val idg = IdGenerator()
    Determinizer.determinize(snfa, Set.empty, Set.empty, idg)
  }

  /**
    * Determinizes a SNFA, while also taking into account declarations for exclusives and extras. The final SDFA should
    * be equivalent to the SDFA produced by fsm.symbolic.sfa.SFAUtils#determinize(fsm.symbolic.sfa.snfa.SNFA), but with
    * possibly fewer transitions and states.
    *
    * @param snfa The SNFA to be determinized.
    * @param exclusives The set of exclusives.
    * @param extras The set of extras.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinize(
                   snfa: SNFA,
                   exclusives: Set[Set[Predicate]],
                   extras: Set[Sentence]
                 ): SDFA = {
    val idg = IdGenerator()
    Determinizer.determinize(snfa, exclusives, extras, idg)
  }

  /**
    * Determinizes a SNFA, while also taking into account declarations for exclusives and extras.
    * This is an incremental version of fsm.symbolic.sfa.SFAUtils#determinize(fsm.symbolic.sfa.snfa.SNFA), i.e.,
    * we incrementally create states that are needed without constructing the powerset.
    *
    * @param snfa The SNFA to be determinized.
    * @param exclusives The set of exclusives.
    * @param extras The set of extras.
    * @param minTermMethod The method used to create the minterms,
    *                       "withsat" creates minterms by checking satisfiability
    *                       "withoutsat" creates minterms faster by bypassing satisfiability but may create more of
    *                       them.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinizeI(
                    snfa: SNFA,
                    exclusives: Set[Set[Predicate]],
                    extras: Set[Sentence],
                    minTermMethod: String
                  ): SDFA = {
    DeterminizerIncr.determinize(snfa, exclusives, extras, minTermMethod)
  }

  /**
    * Same as fsm.symbolic.sfa.SFAUtils#determinizeI(fsm.symbolic.sfa.snfa.SNFA, scala.collection.immutable.Set, scala.collection.immutable.Set, java.lang.String)
    * Uses default methof for minterm construction.
    *
    * @param snfa The SNFA to be determinized.
    * @param exclusives The set of exclusives.
    * @param extras The set of extras.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinizeI(
                    snfa: SNFA,
                    exclusives: Set[Set[Predicate]],
                    extras: Set[Sentence]
                  ): SDFA = {
    DeterminizerIncr.determinize(snfa, exclusives, extras, ConfigUtils.defaultMinTermMethod)
  }

  //TODO: use this in fsm.symbolic.sfa.snfa.SNFAUtils.makeSNFAComplete and add idg as argument
  /**
    * Incrementally determinizes a SNFA.
    *
    * @param snfa The SNFA to be determinized.
    * @return A SDFA equivalent to the given SNFA.
    */
  def determinizeI(snfa: SNFA): SDFA = {
    DeterminizerIncr.determinize(snfa, Set.empty, Set.empty, ConfigUtils.defaultMinTermMethod)
  }

}
