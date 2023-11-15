package workflow.provider

import java.io.{FileInputStream, ObjectInputStream}

import fsm.DFAInterface
import fsm.classical.fa.dfa.{DFA, DFAFactory}
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.CountPolicy.CountPolicy
import fsm.classical.pattern.regexp.RegExpTree
import fsm.classical.pattern.regexp.RegExpUtils.{getPatternSigmaStar, xml2re}
import model.vmm.mapper.Isomorphism
import ui.ConfigUtils
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.dfa.{DFASource, DFASourceDirect, DFASourceFromSDFA, DFASourceFromXML, DFASourceRegExp, DFASourceSerialized}

object DFAProvider {
  /**
    * Constructor for DFA provider when dfaSource is workflow.provider.source.dfa.DFASourceDirect.
    * The source contains already the DFAs. We only need to add ids.
    *
    * @param dfaSource A workflow.provider.source.dfa.DFASourceDirect.
    * @return A DFA provider.
    */
  def apply(dfaSource: DFASourceDirect): DFAProvider = new DFAProvider(dfaSource, List.empty[Condition])

  /**
    * Constructor for DFA provider when dfaSource is workflow.provider.source.dfa.DFASourceRegExp.
    * The source contains the regular expression tree for a DFA.
    *
    * @param dfaSource A workflow.provider.source.dfa.DFASourceRegExp.
    * @return A DFA provider.
    */
  def apply(dfaSource: DFASourceRegExp): DFAProvider = new DFAProvider(dfaSource, List.empty[Condition])

  /**
    * Constructor for DFA provider when dfaSource is workflow.provider.source.dfa.DFASourceFromXML.
    * The source contains the file with the pattern for a DFA.
    *
    * @param dfaSource A workflow.provider.source.dfa.DFASourceFromXML.
    * @return A DFA provider.
    */
  def apply(dfaSource: DFASourceFromXML): DFAProvider =
    new DFAProvider(dfaSource, List(new FileExistsCondition(dfaSource.xmlFile)))

  /**
    * Constructor for DFA provider when dfaSource is workflow.provider.source.dfa.DFASourceSerialized.
    * The source contains the path to the file with the serialized list of DFA interfaces.
    *
    * @param dfaSource A workflow.provider.source.dfa.DFASourceSerialized.
    * @return A DFA provider.
    */
  def apply(dfaSource: DFASourceSerialized): DFAProvider =
    new DFAProvider(dfaSource, List(new FileExistsCondition(dfaSource.fn)))

  /**
    * Constructor for DFA provider when dfaSource is workflow.provider.source.dfa.DFASourceFromSDFA.
    * The source contains a SDFA and an isomorphism based on which a structurally equivalent DFA will be constructed.
    *
    * @param dfaSource A workflow.provider.source.dfa.DFASourceFromSDFA.
    * @return A DFA provider.
    */
  def apply(dfaSource: DFASourceFromSDFA): DFAProvider = new DFAProvider(dfaSource, List.empty)
}

/**
  * According to type of source for DFA, builds a provider in the form of a list of DFA interfaces.
  *
  * @param dfaSource The source for the DFA.
  *                  - DFASourceDirect when an already existing SDFA is available. Mostly used for testing and running
  *                  experiments.
  *                  - DFASourceRegExp when a regular expression is available. Mostly used for testing.
  *                  - DFASourceFromXML when a XML file with the patterns is given.
  *                  - DFASourceSerialized when a serialized DFA is given, used mostly after disambiguation.
  *                  - DFASourceFromSDFA when we want to create a DFA from a SDFA.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class DFAProvider private (
                            dfaSource: DFASource,
                            conditions: List[Condition]
                          ) extends AbstractProvider(conditions) {

  checkConditions()

  /**
    * Calling this function actually initiates the construction of the DFA interfaces.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the XML file) and then call provide(). Keep the
    * source until you call provide().
    *
    * @return A list of DFA interfaces.
    */
  override def provide(): List[DFAInterface] = {
    dfaSource match {
      case x: DFASourceDirect => addIds(x.dfa)
      case x: DFASourceRegExp => re2dfa(x.re, x.policy, x.order, x.streamSymbols, x.partitionAttribute)
      case x: DFASourceFromXML => xml2dfa(x.xmlFile, x.policy, x.order, x.streamSymbols)
      case x: DFASourceSerialized => deserialize(x.fn)
      case x: DFASourceFromSDFA => sdfa2dfa(x.sdfa, x.iso)
      case _ => throw new Error("Not valid DFASource")
    }
  }

  /**
    * Just creates and adds an identifier to each DFA.
    *
    * @param dfas The list of DFAs.
    * @return The list of corresponding DFA with identifiers.
    */
  private def addIds(dfas: List[DFA]): List[DFAInterface] = {
    val dfai = dfas.zipWithIndex
    val fsm = dfai.map(s => DFAInterface(s._1, s._2))
    fsm
  }

  /**
    * Given a regular expression tree, creates an equivalent DFA.
    *
    * @param regExpTree The regular expression tree.
    * @param policy The counting policy.
    * @param order The disambiguation order.
    * @param streamSymbols All the symbols appearing in a stream.
    * @param partitionAttribute The partition attribute.
    * @return The equivalent DFA (interface), as a list.
    */
  private def re2dfa(
                      regExpTree: RegExpTree,
                      policy: CountPolicy,
                      order: Int,
                      streamSymbols: Set[String],
                      partitionAttribute: String
                    ): List[DFAInterface] = {
    val patternSS = getPatternSigmaStar(regExpTree, streamSymbols.map(x => x.toString).toList.toSet)
    val dfa = DFAFactory.buildDFAFromRe(patternSS, policy, streamSymbols, order) //TODO: why re as input, patternSS?
    val dfai = DFAInterface(dfa, 0, partitionAttribute)
    List(dfai)
  }

  /**
    * Constructs a DFA from a pattern given in a XML file.
    *
    * @param fn The path to the XML file.
    * @param policy The counting policy.
    * @param order The disambiguation order.
    * @param streamSymbols All the symbols appearing in a stream.
    * @return The equivalent DFA (interface), as a list.
    */
  private def xml2dfa(
                       fn: String,
                       policy: CountPolicy,
                       order: Int,
                       streamSymbols: Set[String]
                     ): List[DFAInterface] = {
    val rePart = xml2re(fn, streamSymbols)
    val re = rePart._1
    val partitionAttribute = rePart._2
    val dfa = DFAFactory.buildDFAFromRe(re, policy, streamSymbols, order)
    val dfai = DFAInterface(dfa, 0, partitionAttribute)
    List(dfai)
  }

  /**
    * Just deserializes a list of serialized DFA.
    *
    * @param fn The path to the file/directory containing the serialized list of DFA.
    * @return A list of DFA.
    */
  private def deserialize(fn: String): List[DFAInterface] = {
    val oisDFA = new ObjectInputStream(new FileInputStream(fn))
    val dfa = oisDFA.readObject.asInstanceOf[List[DFAInterface]]
    oisDFA.close()
    dfa
  }

  /**
    * Converts a SDFA to a DFA through an isomorphism.
    *
    * @param sdfa The given SDFA.
    * @param iso The given isomorphism.
    * @return The structurally equivalent DFA.
    */
  private def sdfa2dfa(
                        sdfa: SDFA,
                        iso: Isomorphism
                      ): List[DFAInterface] = {
    val dfa = DFAFactory.buildDFAFromSDFA(sdfa, iso)
    val dfai = DFAInterface(dfa, 0, ConfigUtils.singlePartitionVal)
    List(dfai)
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
      throw new Error("Provider conditions unsatisfied")
      false
    } else true
  }
}
