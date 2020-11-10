package workflow.provider

import java.io.{File, FileInputStream, ObjectInputStream}
import com.typesafe.scalalogging.LazyLogging
import fsm.SDFAInterface
import fsm.classical.fa.dfa.DFAUtils
import fsm.classical.fa.nfa.NFAUtils
import fsm.symbolic.sre.{SREFormula, SREUtils}
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.logic.{Predicate, Sentence}
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAUtils}
import fsm.symbolic.sfa.snfa.SNFAUtils
import workflow.condition.{Condition, FileExistsCondition}
import fsm.CountPolicy.CountPolicy
import workflow.provider.source.sdfa.{SDFASource, SDFASourceDirect, SDFASourceDirectI, SDFASourceFormula, SDFASourceFromSRE, SDFASourceLLDFA, SDFASourceSerialized}

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
      case x: SDFASourceFromSRE => sre2sdfa(x.sreFile, x.policy, x.declarations, x.minTermMethod)
      case x: SDFASourceSerialized => deserialize(x.fn)
      case x: SDFASourceLLDFA => lldfa2dfa2sdfa(
        x.states,
        x.transitions,
        x.start,
        x.finals,
        x.streaming,
        x.disambiguate,
        x.order
      )
      case _ => {
        logger.error("Not valid SDFASource")
        throw new Error("Not valid SDFASource")
      }
    }
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
    * Just creates and adds an identifier to each SDFA. Also adds a partitions attribute to each SDFA from the list of
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
                            formulas: List[(SREFormula, Int, String)],
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
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(fn, declarations, withSelection = false)
    formula2sdfa(formulas, policy, exclusives, extras, minTermMethod)
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
                              order: Int
                            ): List[SDFAInterface] = {
    logger.debug("LLDFA has " + states.size + " states and " + finals.size + " finals.")
    logger.info("Building NFA from LLDFA")
    val nfa =
      if (streaming) NFAUtils.lldfa2nfa(states, transitions, start, finals)
      else NFAUtils.lldfa2nfaNoStream(states, transitions, start, finals)
    //logger.debug("NFA has " + nfa.getAllStates.size + " states." + nfa.getAcceptingId.size + " are final.")
    logger.info("Converting NFA to DFA")
    val dfa = {
      val tmpdfa = DFAUtils.convertNfa2Dfa(nfa)
      if (disambiguate) DFAUtils.disambiguate(tmpdfa, order)
      else tmpdfa
    }
    //logger.debug("DFA has " + dfa.getStates.size + " states." + dfa.getAllFinals.size + " are final.")
    logger.info("Converting DFA to SDFA")
    val sdfa = SDFAUtils.dfa2sdfa(dfa)
    logger.debug("SDFA has " + sdfa.states.size + " states and " + sdfa.finals.size + " finals.")
    addIds(List(sdfa))
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
