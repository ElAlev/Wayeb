package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import fsm.DSRAInterface
import fsm.classical.pattern.regexp.RegExpTree
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sra.dsra.{DSRAStreaming, DSRAUtils}
import fsm.symbolic.sre.{SREFormula, SREUtils}
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.dsra.{DSRASource, DSRASourceDirect, DSRASourceDirectI, DSRASourceFromSREM, DSRASourceRegExp, DSRASourceSerialized}
import java.io.{File, FileInputStream, ObjectInputStream}

object DSRAProvider {

  def apply(dsraSource: DSRASourceRegExp): DSRAProvider = new DSRAProvider(dsraSource, List.empty[Condition])

  /**
   * Constructor for DSRA provider when dsraSource is workflow.provider.source.dsra.DSRASourceDirect.
   * The source contains already the DSRAs. We only need to add ids.
   *
   * @param dsraSource  A workflow.provider.source.dsra.DSRASourceDirect.
   * @return            A DSRA provider.
   */
  def apply(dsraSource: DSRASourceDirect): DSRAProvider = new DSRAProvider(dsraSource, List.empty[Condition])

  /**
   * Constructor for DSRA provider when dsraSource is workflow.provider.source.dsra.DSRASourceDirectI.
   * The source contains already the DSRAs. We only need to add ids.
   *
   * @param dsraSource  A workflow.provider.source.dsra.DSRASourceDirectI.
   * @return            A DSRA provider.
   */
  def apply(dsraSource: DSRASourceDirectI): DSRAProvider = new DSRAProvider(dsraSource, List.empty[Condition])

  /**
   * Constructor for dSRA provider when dsraSource is workflow.provider.source.dsra.DSRASourceFromSREM.
   * The source contains the path to the file with the patterns.
   *
   * @param dsraSource  A workflow.provider.source.dsra.DSRASourceFromSREM.
   * @return            A dSRA provider.
   */
  def apply(dsraSource: DSRASourceFromSREM): DSRAProvider =
    new DSRAProvider(dsraSource, List(new FileExistsCondition(dsraSource.sreFile)))

  /**
   * Constructor for dSRA provider when dsraSource is workflow.provider.source.dsra.DSRASourceSerialized.
   * The source contains the path to the file with the serialized list of dSRA interfaces.
   *
   * @param dsraSource  A workflow.provider.source.dsra.DSRASourceSerialized.
   * @return            A dSRA provider.
   */
  def apply(
             dsraSource: DSRASourceSerialized
           ): DSRAProvider = new DSRAProvider(dsraSource, List(new FileExistsCondition(dsraSource.fn)))
}

/**
 * According to type of source for dSRA, builds a provider in the form of a list of dSRA interfaces.
 *
 * @param dsraSource The source for the dSRA.
 *                     - DSRASourceDirect/DSRASourceDirectI when an already existing dSRA is available.
 *                     Mostly used for testing and running experiments.
 *                     - DSRASourceFromSREM when a file with patterns is given.
 *                     - DSRASourceSerialized when a serialized dSRA is given.
 *                     - DSRASourceRegExp when a regular expression is given.
 * @param conditions A list of conditions that must be checked and satisfied.
 */
class DSRAProvider private (
                           dsraSource: DSRASource,
                           conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {
  checkConditions()

  /**
   * Calling this function actually initiates the construction of the DSRA interfaces.
   * Before calling this, nothing is done.
   * CAUTION: Do not create a provider, then delete its source (e.g. the SREM file) and then call provide(). Keep the
   * source until you call provide().
   *
   * @return A list of DSRA interfaces.
   */
  override def provide(): List[DSRAInterface] = {
    dsraSource match {
      case x: DSRASourceDirectI => x.dsrai
      case x: DSRASourceFromSREM => srem2dsra(x.sreFile, x.declarations)
      case x: DSRASourceSerialized => deserialize(x.fn)
      case x: DSRASourceDirect =>
        if (x.partitionAttributes.isEmpty) addIds(x.dsra)
        else addIds(x.dsra, x.partitionAttributes)
      case x: DSRASourceRegExp => re2dsra(x.re, x.partitionAttribute, x.window)
      case _ => {
        logger.error("Not valid DSRASource")
        throw new Error("Not valid DSRASource")
      }
    }
  }

  /**
   * Given a regular expression tree, creates an equivalent DSRA.
   *
   * @param re                 The regular expression tree.
   * @param partitionAttribute The partition attribute.
   * @param window             The window.
   * @return                   The equivalent DSRA (interface), as a list.
   */
  private def re2dsra(
                       re: RegExpTree,
                       partitionAttribute: String,
                       window: Int
                     ): List[DSRAInterface] = {
    val formula = SREUtils.re2formula(re)
    formula2dsra(List((formula,0,partitionAttribute,window,"count")), Set.empty, Set.empty)
  }

  /**
   * From a set of patterns, creates a list of corresponding dSRA.
   * First parses patterns into formulas and declarations and then provides those to
   * workflow.provider.DSRAProvider#formula2dsra(scala.collection.immutable.List, scala.collection.immutable.Set, scala.collection.immutable.Set)
   *
   * @param fn            The file containing the patterns.
   * @param declarations  The file containing the declarations, i.e., extras and/or exclusives. Not currently used.
   * @return              A list of corresponding dSRA.
   */
  private def srem2dsra(
                         fn: String,
                         declarations: String
                       ): List[DSRAInterface] = {
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(fn, declarations, withSelection = true)
    formula2dsra(formulas, exclusives, extras)
  }

  /**
   * Given a list of formulas, extras and exclusives, builds the actual streaming dSRA.
   *
   * @param formulas      The list of formulas.
   * @param exclusives    not currently used.
   * @param extras        not currently used.
   * @return              A list of dSRA.
   */
  private def formula2dsra(
                            formulas: List[(SREFormula, Int, String, Int, String)],
                            exclusives: Set[Set[Predicate]],
                            extras: Set[Sentence]
                          ): List[DSRAInterface] = {
    logger.info("Building DSRA")
    val dsra = formulas.map(f => (DSRAUtils.buildDSRAForStream(f._1, f._4)))
    val parAttr = formulas.map(f => f._3)
    addIds(dsras = dsra, partitionAttributes = parAttr)
  }

  /**
   * Just creates and adds an identifier to each dSRA.
   *
   * @param dsras The list of dSRAs.
   * @return      The list of corresponding dSRA with identifiers.
   */
  private def addIds(dsras: List[DSRAStreaming]): List[DSRAInterface] = {
    val dsrai = dsras.zipWithIndex
    val fsm = dsrai.map(s => DSRAInterface(s._1, s._2))
    fsm
  }

  /**
   * Just creates and adds an identifier to each DSRA. Also adds a partitions attribute to each DSRA from the list of
   * given attributes.
   *
   * @param dsras               The list of DSRAs.
   * @param partitionAttributes The list of given partition attributes.
   * @return                    The list of corresponding DSRA with identifiers and partition attributes.
   */
  private def addIds(
                      dsras: List[DSRAStreaming],
                      partitionAttributes: List[String]
                    ): List[DSRAInterface] = {
    require(dsras.size == partitionAttributes.size)
    logger.debug("Adding IDs to DSRAs...")
    val dsrap = dsras.zip(partitionAttributes)
    val dsraip = dsrap.zipWithIndex
    val fsm = dsraip.map(
      s => DSRAInterface(
        dsra               = s._1._1,
        id = s._2,
        partitionAttribute = s._1._2
      )
    )
    fsm
  }

  /**
   * Just deserializes a list of serialized DSRA.
   *
   * @param fn The path to the file/directory containing the serialized list of DSRA.
   *           If file, directly deserialize. If directory, gather all files, deserialize them all and give new
   *           identifiers to avoid id conflicts.
   * @return   A list of DSRA
   */
  private def deserialize(fn: String): List[DSRAInterface] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val sdfas = files.flatMap(f => deserializeFile(fn + "/" + f))
      resetIds(sdfas)
    } else throw new Error("Something wrong with given DSRAs file/directory.")
  }

  /**
   * Actual deserialization from a single file.
   *
   * @param fn The path to the file containing the serialized list of DSRA.
   * @return   A list of DSRA.
   */
  private def deserializeFile(fn: String): List[DSRAInterface] = {
    val oisDSRA = new ObjectInputStream(new FileInputStream(fn))
    val dsra = oisDSRA.readObject.asInstanceOf[List[DSRAInterface]]
    oisDSRA.close()
    dsra
  }

  /**
   * Discards the ids of a list of DSRA interfaces and creates new ones.
   *
   * @param dsras The list of the DSRA interfaces.
   * @return      The list of the DSRA interfaces, with new ids.
   */
  private def resetIds(dsras: List[DSRAInterface]): List[DSRAInterface] = {
    dsras.zipWithIndex.map(
      si => DSRAInterface(
        dsra               = si._1.dsra,
        id = si._2,
        partitionAttribute = si._1.partitionAttribute
      )
    )
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
