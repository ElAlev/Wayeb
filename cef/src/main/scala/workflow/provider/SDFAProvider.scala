package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy.CountPolicy
import fsm.SDFAInterface
import fsm.classical.fa.dfa.{DFA, DFAUtils}
import fsm.classical.fa.nfa.NFAUtils
import fsm.classical.pattern.regexp.RegExpTree
import fsm.symbolic.logic.{AtomicSentence, Predicate, PredicateConstructor, Sentence}
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAUtils}
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre.{SREFormula, SREUtils}
import model.vmm.mapper.Isomorphism
import ui.ConfigUtils
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.sdfa._

import java.io.{File, FileInputStream, ObjectInputStream}

object SDFAProvider {
  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceDirectI.
    * The source contains already the SDFA interfaces.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceDirectI.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceDirectI): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceDirect.
    * The source contains already the SDFAs. We only need to add ids.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceDirect.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceDirect): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceRegExp.
    * The source contains the regular expression tree for a single SDFA.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceRegExp.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceRegExp): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])

  /**
   * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceFormula.
   * The source contains the formulas for the SDFAs.
   *
   * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceFormula.
   * @return A SDFA provider.
   */
  def apply(sdfaSource: SDFASourceFormula): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceFromSRE.
    * The source contains the path to the file with the patterns.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceFromSRE.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceFromSRE): SDFAProvider =
    new SDFAProvider(sdfaSource, List(new FileExistsCondition(sdfaSource.sreFile)))

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceSerialized.
    * The source contains the path to the file with the serialized list of SDFA interfaces.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceSerialized.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceSerialized): SDFAProvider =
    new SDFAProvider(sdfaSource, List(new FileExistsCondition(sdfaSource.fn)))

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceDFA.
    * The source contains a DFA and an isomorphism. The SDFA will be structurally equivalent to the DFA and have the
    * isomorphism's minterms on its transitions.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceDFA.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceDFA): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])

  /**
    * Constructor for SDFA provider when sdfaSource is workflow.provider.source.sdfa.SDFASourceLLDFA.
    * The source contains a LearnLib DFA.
    *
    * @param sdfaSource A workflow.provider.source.sdfa.SDFASourceLLDFA.
    * @return A SDFA provider.
    */
  def apply(sdfaSource: SDFASourceLLDFA): SDFAProvider = new SDFAProvider(sdfaSource, List.empty[Condition])
}

/**
  * According to type of source for SDFA, builds a provider in the form of a list of SDFA interfaces.
  *
  * @param sdfaSource The source for the SDFA.
  *                     - SDFASourceDirect/SDFASourceDirectI when an already existing SDFA is available.
  *                     Mostly used for testing and running experiments.
  *                     - SDFASourceFormula when a formula is available, again used mostly for testing.
  *                     - SDFASourceFromSRE when a file with patterns is given.
  *                     - SDFASourceSerialized when a serialized SDFA is given, used mostly after disambiguation.
  *                     - SDFASourceDFA when a classical DFA is provided. Useful when we have too many symbols. Faster
  *                     to create first a DFA and then convert it to a SDFA.
  *                     - SDFASourceLLDFA when a classical DFA from LearnLib is provided. Only a single LLDFA can be
  *                     provided for now.
  * @param conditions A list of conditions that must be checked and satisfied.
  */

class SDFAProvider private (
                             sdfaSource: SDFASource,
                             conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {
  checkConditions()

  /**
    * Calling this function actually initiates the construction of the SDFA interfaces.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the SRE file) and then call provide(). Keep the
    * source until you call provide().
    *
    * @return A list of SDFA interfaces.
    */
  override def provide(): List[SDFAInterface] = {
    sdfaSource match {
      case x: SDFASourceDirectI => x.sdfai
      case x: SDFASourceDirect =>
        if (x.partitionAttributes.isEmpty) addIds(x.sdfa)
        else addIds(x.sdfa, x.partitionAttributes)
      case x: SDFASourceFormula => formula2sdfa(x.formulas, x.policy, x.exclusives, x.extras, x.minTermMethod)
      case x: SDFASourceRegExp => re2sdfa(x.re,x.order,x.partitionAttribute,x.exclusives,x.extras,x.policy,x.minTermMethod)
      case x: SDFASourceFromSRE => sre2sdfa(x.sreFile, x.policy, x.declarations, x.minTermMethod)
      case x: SDFASourceSerialized => deserialize(x.fn)
      case x: SDFASourceDFA => dfa2sdfa(x.dfa, x.iso)
      case x: SDFASourceLLDFA => lldfa2dfa2sdfa(
        x.states,
        x.transitions,
        x.start,
        x.finals,
        x.streaming,
        x.disambiguate,
        x.order,
        x.partitionAttribute
      )
      case _ => {
        logger.error("Not valid SDFASource")
        throw new Error("Not valid SDFASource")
      }
    }
  }

  /**
   * Given a regular expression tree, creates an equivalent SDFA.
   *
   * @param re                 The regular expression tree.
   * @param order              The disambiguation order.
   * @param partitionAttribute The partition attribute.
   * @param policy             The counting policy.
   * @param exclusives         Sets of exclusive predicates.
   * @param extras             Sets of extra predicates.
   * @param minTermMethod      The method for constructing the minterms, withsat or withoutsat.
   * @return The equivalent SDFA (interface), as a list.
   */
  private def re2sdfa(
                       re: RegExpTree,
                       order: Int,
                       partitionAttribute: String,
                       exclusives: Set[Set[Predicate]],
                       extras: Set[Sentence],
                       policy: CountPolicy,
                       minTermMethod: String
                     ): List[SDFAInterface] = {

    val formula = SREUtils.re2formula(re)
    formula2sdfa(List((formula, order, partitionAttribute, 0, "count")),policy,exclusives,extras,minTermMethod)
  }

  /**
    * Just creates and adds an identifier to each SDFA.
    *
    * @param sdfas The list of SDFAs.
    * @return The list of corresponding SDFA with identifiers.
    */
  private def addIds(sdfas: List[SDFA]): List[SDFAInterface] = {
    val sdfai = sdfas.zipWithIndex
    val fsm = sdfai.map(s => SDFAInterface(s._1, s._2))
    fsm
  }

  /**
    * Just creates and adds an identifier to each SDFA. Also adds a partition attribute to each SDFA from the list of
    * given attributes.
    *
    * @param sdfas The list of SDFAs.
    * @param partitionAttributes The list of given partition attributes.
    * @return The list of corresponding SDFA with identifiers and partition attributes.
    */
  private def addIds(
                      sdfas: List[SDFA],
                      partitionAttributes: List[String]
                    ): List[SDFAInterface] = {
    require(sdfas.size == partitionAttributes.size)
    logger.debug("Adding IDs to SDFAs...")
    val sdfap = sdfas.zip(partitionAttributes)
    val sdfaip = sdfap.zipWithIndex
    val fsm = sdfaip.map(
      s => SDFAInterface(
        sdfa               = s._1._1,
        id = s._2,
        partitionAttribute = s._1._2
      )
    )
    fsm
  }

  /**
    * Given a list of formulas, extras and exclusives, builds the actual SDFA.
    * First creates SNFA (with minor modification for streams), then goes through determinization.
    * Finally, the SDFA are disambiguated.
    *
    * @param formulas The list of formulas.
    * @param policy Counting policy.
    * @param exclusives Sets of exclusive predicates.
    * @param extras Sets of extra predicates.
    * @param minTermMethod The method for constructing the minterms, withsat or withoutsat.
    * @return a list of SDFA
    */
  private def formula2sdfa(
                            formulas: List[(SREFormula, Int, String, Int, String)],
                            policy: CountPolicy,
                            exclusives: Set[Set[Predicate]],
                            extras: Set[Sentence],
                            minTermMethod: String
                          ): List[SDFAInterface] = {
    logger.info("Building SNFA")
    val snfaStream = formulas.map(f => (SNFAUtils.buildSNFAForStream(f._1), f._2))
    logger.info("Determinizing")
    val sdfa = snfaStream.map(s => (SFAUtils.determinizeI(s._1, exclusives, extras, minTermMethod), s._2))
    sdfa.foreach(s => SDFAUtils.checkForDead(s._1))
    val sdfap = sdfa.map(s => (SDFAUtils.setPolicy(s._1, policy), s._2))
    logger.info("Disambiguating")
    val t1 = System.nanoTime()
    val sdfaDis = sdfap.map(s => SDFAUtils.disambiguateMutant(s._1, s._2))
    val t2 = System.nanoTime()
    val td = (t2 - t1) / 1000000
    logger.debug("Disambiguation time: " + td)
    val parAttr = formulas.map(f => f._3)
    addIds(sdfas               = sdfaDis, partitionAttributes = parAttr)
  }

  /**
    * From a set of patterns, creates a list of corresponding SDFA.
    * First parses patterns into formulas and declarations and then provides those to
    * workflow.provider.SDFAProvider#formula2sdfa(scala.collection.immutable.List, scala.Enumeration.Value, scala.collection.immutable.Set, scala.collection.immutable.Set)
    *
    * @param fn The file containing the patterns.
    * @param policy Counting policy, overlap or non-overlap.
    * @param declarations The file containing the declarations, i.e., extras and/or exclusives.
    * @return list of corresponding SDFA
    */
  private def sre2sdfa(
                        fn: String,
                        policy: CountPolicy,
                        declarations: String,
                        minTermMethod: String
                      ): List[SDFAInterface] = {
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(fn, declarations, withSelection = true)
    formula2sdfa(formulas, policy, exclusives, extras, minTermMethod)
  }

  /**
    * Converts a classical DFA to a SDFA, according to the provided isomorphism.
    *
    * @param dfa The original, classical DFA.
    * @param iso The isomorphism, mapping each classical symbol to a minterm and vice versa.
    * @return The "equivalent" SDFA interface, given as a list.
    */
  private def dfa2sdfa(
                        dfa: DFA,
                        iso: Isomorphism
                      ): List[SDFAInterface] = {
    val sdfa = SDFAUtils.dfa2sdfa(dfa, iso)
    addIds(List(sdfa))
  }

  /**
    * Creates a SDFA from a LearnLib DFA. We first convert the LLDFA to a stream NFA. Then determinize the stream NFA.
    * Finally, we convert the DFA to a SDFA. Performs no disambiguation.
    *
    * @param states The set of state ids.
    * @param transitions The set of transitions as (source,target,symbol) tuples.
    * @param start The id of the start state.
    * @param finals The ids of the final states.
    * @return The final SDFA (not disambiguated).
    */
  private def lldfa2dfa2sdfa(
                              states: Set[Int],
                              transitions: Set[(Int, Int, String)],
                              start: Int,
                              finals: Set[Int],
                              streaming: Boolean,
                              disambiguate: Boolean,
                              order: Int,
                              partitionAttribute: String
                            ): List[SDFAInterface] = {
    logger.debug("LLDFA has " + states.size + " states and " + finals.size + " finals.")
    logger.info("Building NFA from LLDFA")
    val nfa =
      if (streaming) NFAUtils.lldfa2nfa(states, transitions, start, finals)
      else NFAUtils.lldfa2nfaNoStream(states, transitions, start, finals)
    //logger.debug("NFA has " + nfa.getAllStates.size + " states." + nfa.getAcceptingId.size + " are final.")
    logger.info("Converting NFA to DFA")
    val dfa = {
      val allSymbols: Set[String] = transitions.map(t => t._3)
      val tmpnfa = NFAUtils.eliminateEpsilon(nfa,allSymbols)
      val tmpdfa = DFAUtils.convertNfa2Dfa(tmpnfa)
      if (disambiguate) DFAUtils.disambiguate(tmpdfa, order)
      else tmpdfa
    }
    //logger.debug("DFA has " + dfa.getStates.size + " states." + dfa.getAllFinals.size + " are final.")
    logger.info("Converting DFA to SDFA")
    val sdfa = SDFAUtils.dfa2sdfa(dfa)
    logger.debug("SDFA has " + sdfa.states.size + " states and " + sdfa.finals.size + " finals.")
    addIds(List(sdfa), List(partitionAttribute))
  }

  /**
    * Just deserializes a list of serialized SDFA.
    *
    * @param fn The path to the file/directory containing the serialized list of SDFA.
    *           If file, directly deserialize. If directory, gather all files, deserialize them all and give new
    *           identifiers to avoid id conflicts.
    * @return A list of SDFA
    */
  private def deserialize(fn: String): List[SDFAInterface] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val sdfas = files.flatMap(f => deserializeFile(fn + "/" + f))
      resetIds(sdfas)
    } else throw new Error("Something wrong with given SDFAs file/directory.")
  }

  /**
    * Actual deserialization from a single file.
    *
    * @param fn The path to the file containing the serialized list of SDFA.
    * @return a list of SDFA
    */
  private def deserializeFile(fn: String): List[SDFAInterface] = {
    val oisDFA = new ObjectInputStream(new FileInputStream(fn))
    val sdfa = oisDFA.readObject.asInstanceOf[List[SDFAInterface]]
    oisDFA.close()
    sdfa
  }

  /**
    * Discards the ids of a list of SDFA interfaces and creates new ones.
    *
    * @param sdfas The list of the SDFA interfaces.
    * @return The list of the SDFA interfaces, with new ids.
    */
  private def resetIds(sdfas: List[SDFAInterface]): List[SDFAInterface] = {
    sdfas.zipWithIndex.map(
      si => SDFAInterface(
        sdfa               = si._1.sdfa,
        id = si._2,
        partitionAttribute = si._1.partitionAttribute
      )
    )
  }

  /**
    * Creates a SDFA from a LearnLib DFA. We first convert the LLDFA to a stream SNFA. Then we create the set of
    * exclusives and add RESET as an extra minterm. We then determinize the SNFA and finally disambiguate it.
    * CAUTION: For large automata, this method might be too slow.
    * Use workflow.provider.SDFAProvider#lldfa2dfa2sdfa(scala.collection.immutable.Set, scala.collection.immutable.Set, int, scala.collection.immutable.Set, int)
    * in these cases.
    *
    * @param states The set of state ids.
    * @param transitions The set of transitions as (source,target,symbol) tuples.
    * @param start The id of the start state.
    * @param finals The ids of the final states.
    * @param order The order of the disambiguated SDFA.
    * @return The final, disambiguated SDFA.
    */
  private def lldfa2sdfa(
                          states: Set[Int],
                          transitions: Set[(Int, Int, String)],
                          start: Int,
                          finals: Set[Int],
                          order: Int
                        ): List[SDFAInterface] = {
    logger.info("Building SNFA from LLDFA")
    // First create the SNFA. We do not directly convert the LLDFA to a SDFA because we need to take into account the
    // fact that symbols are mutually exclusive. We also need to add the RESET "symbol".
    val snfaStream = SNFAUtils.buildStreamSNFAFromLLDFA(states, transitions, start, finals)
    val resetPredicate = PredicateConstructor.getEventTypePred("RESET")
    val resetSentence = AtomicSentence(resetPredicate).asInstanceOf[Sentence]
    // all symbols and RESET are mutually exclusive
    val predicates = transitions.map(t => t._3).map(symbol => PredicateConstructor.getEventTypePred(symbol)) +
      resetPredicate
    val exclusives = Set(predicates)
    // we also add RESET as an extra symbol
    val extras = Set(resetSentence)
    logger.info("Determinizing")
    val sdfa = SFAUtils.determinizeI(snfaStream, exclusives, extras, ConfigUtils.defaultMinTermMethod)
    SDFAUtils.checkForDead(sdfa)
    val sdfaWithPolicy = SDFAUtils.setPolicy(sdfa, ConfigUtils.defaultPolicy)
    logger.info("Disambiguating")
    val t1 = System.nanoTime()
    val sdfaDis = SDFAUtils.disambiguateMutant(sdfaWithPolicy, order)
    val t2 = System.nanoTime()
    val td = (t2 - t1) / 1000000
    logger.debug("Disambiguation time: " + td)
    addIds(List(sdfaDis))
  }

  /**
    * Checks all conditions.
    * TODO: do the checking in the super-class workflow.provider.AbstractProvider?
    *
    * @return True if all conditions satisfied.
    */
  def checkConditions(): Boolean = {
    val superchecks = super.check()
    if (superchecks.contains(false)) {
      logger.error("Provider conditions unsatisfied")
      throw new Error("Provider conditions unsatisfied")
      false
    } else true
  }

}
