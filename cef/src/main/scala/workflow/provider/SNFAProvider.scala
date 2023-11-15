package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import fsm.SNFAInterface
import fsm.classical.pattern.regexp.RegExpTree
import fsm.symbolic.sfa.snfa.{SNFA, SNFAUtils}
import fsm.symbolic.sre.{SREFormula, SREUtils}
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.snfa.{SNFASource, SNFASourceFromSRE, SNFASourceRegExp, SNFASourceSerialized}

import java.io.{File, FileInputStream, ObjectInputStream}

object SNFAProvider {

  /**
   * Constructor for SNFA provider when sdfaSource is workflow.provider.source.snfa.SNFASourceRegExp.
   * The source contains the regular expression tree for a single SNFA.
   *
   * @param snfaSource A workflow.provider.source.snfa.SNFASourceRegExp.
   * @return           A SNFA provider.
   */
  def apply(snfaSource: SNFASourceRegExp): SNFAProvider = new SNFAProvider(snfaSource, List.empty)

  /**
   * Constructor for SNFA provider when snfaSource is workflow.provider.source.snfa.SNFASourceFromSRE.
   * The source contains the path to the file with the patterns.
   *
   * @param snfaSource A workflow.provider.source.snfa.SNFASourceFromSRE.
   * @return           A SNFA provider.
   */
  def apply(snfaSource: SNFASourceFromSRE): SNFAProvider = new SNFAProvider(snfaSource, List(new FileExistsCondition(snfaSource.sreFile)))

  /**
   * Constructor for SNFA provider when snfaSource is workflow.provider.source.snfa.SNFASourceSerialized.
   * The source contains the path to the file with the serialized list of SNFA interfaces.
   *
   * @param snfaSource A workflow.provider.source.snfa.SNFASourceSerialized.
   * @return           A SNFA provider.
   */
  def apply(snfaSource: SNFASourceSerialized): SNFAProvider = new SNFAProvider(snfaSource, List(new FileExistsCondition(snfaSource.fn)))
}

/**
 * According to type of source for SNFA, builds a provider in the form of a list of SNFA interfaces.
 *
 * @param snfaSource The source for the SNFA.
 *                     - SNFASourceFromSRE when a file with patterns is given.
 *                     - SNFASourceSerialized when a serialized dSRA is given.
 *                     - SNFASourceRegExp when a regular expression is given.
 * @param conditions A list of conditions that must be checked and satisfied.
 */
class SNFAProvider private (
                             snfaSource: SNFASource,
                             conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {
  checkConditions()

  /**
   * Calling this function actually initiates the construction of the SNFA interfaces.
   * Before calling this, nothing is done.
   * CAUTION: Do not create a provider, then delete its source (e.g. the SRE file) and then call provide(). Keep the
   * source until you call provide().
   *
   * @return A list of SNFA interfaces.
   */
  override def provide(): List[SNFAInterface] = {
    snfaSource match {
      case x: SNFASourceFromSRE => sre2snfa(x.sreFile)
      case x: SNFASourceSerialized => deserialize(x.fn)
      case x: SNFASourceRegExp => re2snfa(x.re, x.partitionAttribute, x.window)
      case _ => {
        logger.error("Not valid SNFASource")
        throw new Error("Not valid SNFASource")
      }
    }
  }

  /**
   * Given a regular expression tree, creates an equivalent SNFA.
   *
   * @param regExpTree         The regular expression tree.
   * @param partitionAttribute The partition attribute.
   * @param window             The window.
   * @return                   The equivalent SNFA (interface), as a list.
   */
  private def re2snfa(
                       regExpTree: RegExpTree,
                       partitionAttribute: String,
                       window: Int
                     ): List[SNFAInterface] = {
    val formula = SREUtils.re2formula(regExpTree)
    val snfaStream = SNFAUtils.buildSNFAForStream(formula)
    val snfaNoEpsilon = SNFAUtils.eliminateEpsilon(snfaStream)
    val snfai = SNFAInterface(snfaNoEpsilon,1,partitionAttribute,window)
    List(snfai)
  }

  /**
   * From a set of patterns, creates a list of corresponding SNFA.
   * First parses patterns into formulas and declarations and then provides those to
   * workflow.provider.SNFAProvider#formula2snfa(scala.collection.immutable.List)
   *
   * @param fn The file containing the patterns.
   * @return   List of corresponding SNFA.
   */
  private def sre2snfa(fn: String): List[SNFAInterface] = {
    val (formulas, _, _) = SREUtils.sre2formulas(fn, "", withSelection = true)
    formula2snfa(formulas)
  }

  /**
   * Given a list of formulas, extras and exclusives, builds the actual SNFA.
   *
   * @param formulas The list of formulas, as a list of (formula, order, partiotionAttribute, window).
   * @return         A list of SNFA.
   */
  private def formula2snfa(formulas: List[(SREFormula, Int, String, Int, String)]): List[SNFAInterface] = {
    logger.info("Building SNFA")
    val snfaStream = formulas.map(f => (SNFAUtils.buildSNFAForStream(f._1), f._2))
    val snfaNoEpsilon = snfaStream.map(a => SNFAUtils.eliminateEpsilon(a._1))
    val parAttr = formulas.map(f => f._3)
    val windows = formulas.map(f => f._4)
    val windowTypes = formulas.map(f => f._5)
    addIds(snfas               = snfaNoEpsilon, partitionAttributes = parAttr, windows = windows, windowTypes = windowTypes)
  }

  /**
   * Just creates and adds an identifier to each SNFA. Also adds a partition attribute to each SNFA from the list of
   * given attributes.
   *
   * @param snfas               The list of SNFAs.
   * @param partitionAttributes The list of given partition attributes.
   * @param windows             The list of windows.
   * @return                    The list of corresponding SNFAs with identifiers and partition attributes.
   */
  private def addIds(
                      snfas: List[SNFA],
                      partitionAttributes: List[String],
                      windows: List[Int],
                      windowTypes: List[String]
                    ): List[SNFAInterface] = {
    require(snfas.size == partitionAttributes.size)
    logger.debug("Adding IDs to SNFAs...")
    val snfap = snfas.zip(partitionAttributes).zip(windows.zip(windowTypes))
    val snfaip = snfap.zipWithIndex
    val fsm = snfaip.map(
      s => SNFAInterface(
        snfa               = s._1._1._1,
        id = s._2,
        partitionAttribute = s._1._1._2,
        window = s._1._2._1,
        windowTypeStr = s._1._2._2
      )
    )
    fsm
  }

  /**
   * Just deserializes a list of serialized SNFAs.
   *
   * @param fn The path to the file/directory containing the serialized list of SNFAs.
   *           If file, directly deserialize. If directory, gather all files, deserialize them all and give new
   *           identifiers to avoid id conflicts.
   * @return   A list of SNFAs.
   */
  private def deserialize(fn: String): List[SNFAInterface] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val snfas = files.flatMap(f => deserializeFile(fn + "/" + f))
      resetIds(snfas)
    } else throw new Error("Something wrong with given SNFAs file/directory.")
  }

  /**
   * Discards the ids of a list of SNFAs interfaces and creates new ones.
   *
   * @param snfas The list of the SNFA interfaces.
   * @return      The list of the SNFA interfaces, with new ids.
   */
  private def resetIds(snfas: List[SNFAInterface]): List[SNFAInterface] = {
    snfas.zipWithIndex.map(
      si => SNFAInterface(
        snfa               = si._1.snfa,
        id = si._2,
        partitionAttribute = si._1.partitionAttribute
      )
    )
  }

  /**
   * Actual deserialization from a single file.
   *
   * @param fn The path to the file containing the serialized list of SNFAs.
   * @return   A list of SNFAs.
   */
  private def deserializeFile(fn: String): List[SNFAInterface] = {
    val oisNFA = new ObjectInputStream(new FileInputStream(fn))
    val snfa = oisNFA.readObject.asInstanceOf[List[SNFAInterface]]
    oisNFA.close()
    snfa
  }

  def checkConditions(): Boolean = {
    val superchecks = super.check()
    if (superchecks.contains(false)) {
      logger.error("Provider conditions unsatisfied")
      throw new Error("Provider conditions unsatisfied")
      false
    } else true
  }

}
