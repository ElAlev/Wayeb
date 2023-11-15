package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sra.nsra.{NSRA, NSRAUtils}
import fsm.symbolic.sre.{SREFormula, SREUtils}
import fsm.NSRAInterface
import fsm.classical.pattern.regexp.RegExpTree
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.nsra.{NSRASource, NSRASourceFromSREM, NSRASourceRegExp, NSRASourceSerialized}
import java.io.{File, FileInputStream, ObjectInputStream}

object NSRAProvider {
  /**
   * Constructor for NSRA provider when nsraSource is workflow.provider.source.nsra.NSRASourceFromSREM.
   * The source contains the path to the file with the patterns.
   *
   * @param nsraSource A workflow.provider.source.nsra.NSRASourceFromSREM.
   * @return           A NSRA provider.
   */
  def apply(nsraSource: NSRASourceFromSREM): NSRAProvider = new NSRAProvider(nsraSource, List(new FileExistsCondition(nsraSource.sremFile)))

  /**
   * Constructor for SDFA provider when nsraSource is workflow.provider.source.nsra.NSRASourceSerialized.
   * The source contains the path to the file with the serialized list of SDFA interfaces.
   *
   * @param nsraSource A workflow.provider.source.nsra.NSRASourceSerialized.
   * @return           A SDFA provider.
   */
  def apply(nsraSource: NSRASourceSerialized): NSRAProvider = new NSRAProvider(nsraSource, List(new FileExistsCondition(nsraSource.fn)))

  /**
   * Constructor for NSRA provider when nsraSource is workflow.provider.source.nsra.NSRASourceRegExp.
   * The source contains the regular expression tree for a single NSRA.
   *
   * @param nsraSource A workflow.provider.source.nsra.NSRASourceRegExp.
   * @return           A NSRA provider.
   */
  def apply(nsraSource: NSRASourceRegExp): NSRAProvider = new NSRAProvider(nsraSource, List.empty)
}

/**
 * According to type of source for nSRA, builds a provider in the form of a list of nSRA interfaces.
 *
 * @param nsraSource The source for the nSRA.
 *                     - NSRASourceFromSREM when a file with patterns is given.
 *                     - NSRASourceSerialized when a serialized dSRA is given.
 *                     - NSRASourceRegExp when a regular expression is given.
 * @param conditions A list of conditions that must be checked and satisfied.
 */
class NSRAProvider private (
                             nsraSource: NSRASource,
                             conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {

  checkConditions()

  /**
   * Calling this function actually initiates the construction of the NSRA interfaces.
   * Before calling this, nothing is done.
   * CAUTION: Do not create a provider, then delete its source (e.g. the SRE file) and then call provide(). Keep the
   * source until you call provide().
   *
   * @return A list of NSRA interfaces.
   */
  override def provide(): List[NSRAInterface] = {
    nsraSource match {
      case x: NSRASourceSerialized => deserialize(x.fn)
      case x: NSRASourceFromSREM => srem2nsra(x.sremFile)
      case x: NSRASourceRegExp => re2nsra(x.re, x.partitionAttribute, x.window)
      case _ => {
        logger.error("Not valid NSRASource")
        throw new Error("Not valid NSRASource")
      }
    }
  }

  /**
   * Given a regular expression tree, creates an equivalent NSRA.
   *
   * @param re                 The regular expression tree.
   * @param partitionAttribute The partition attribute.
   * @param window             The window.
   * @return                   The equivalent NSRA (interface), as a list.
   */
  private def re2nsra(
                       re: RegExpTree,
                       partitionAttribute: String,
                       window: Int
                     ): List[NSRAInterface] = {
    val formula = SREUtils.re2formula(re)
    val nsra = NSRAUtils.buildStreamingNSRANoWindow(formula)
    val nsrai = NSRAInterface(nsra, 1, partitionAttribute, window)
    List(nsrai)
  }

  /**
   * From a set of patterns, creates a list of corresponding nSRA.
   * First parses patterns into formulas and declarations and then provides those to
   * workflow.provider.NSRAProvider#formula2nsra(scala.collection.immutable.List, scala.collection.immutable.Set, scala.collection.immutable.Set)
   *
   * @param fn           The file containing the patterns.
   * @return list of corresponding nSRA.
   */
  private def srem2nsra(fn: String): List[NSRAInterface] = {
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(fn, "", withSelection = true)
    formula2nsra(formulas, exclusives, extras)
  }

  /**
   * Given a list of formulas, extras and exclusives, builds the actual nSRA.
   *
   * @param formulas   The list of formulas.
   * @param exclusives not currently used.
   * @param extras     not currently used.
   * @return a list of nSRA
   */
  private def formula2nsra(
                            formulas: List[(SREFormula, Int, String, Int, String)],
                            exclusives: Set[Set[Predicate]],
                            extras: Set[Sentence]
                          ): List[NSRAInterface] = {
    logger.info("Building NSRA")
    val nsraStream = formulas.map(f => NSRAUtils.buildStreamingNSRANoWindow(f._1))
    val parAttr = formulas.map(f => f._3)
    val windows = formulas.map(f => f._4)
    val windowTypes = formulas.map(f => f._5)
    addIds(nsras = nsraStream, partitionAttributes = parAttr, windows = windows, windowTypes = windowTypes)
  }

  /**
   * Just creates and adds an identifier to each NSRA. Also adds a partition attribute to each NSRA from the list of
   * given attributes.
   *
   * @param nsras               The list of NSRAs.
   * @param partitionAttributes The list of given partition attributes.
   * @param windows             The list of windows.
   * @return                    The list of corresponding NSRA with identifiers and partition attributes.
   */
  private def addIds(
                      nsras: List[NSRA],
                      partitionAttributes: List[String],
                      windows: List[Int],
                      windowTypes: List[String]
                    ): List[NSRAInterface] = {
    require(nsras.size == partitionAttributes.size)
    logger.debug("Adding IDs to NSRAs...")
    val nsrap = nsras.zip(partitionAttributes).zip(windows.zip(windowTypes))
    val nsraip = nsrap.zipWithIndex
    val fsm = nsraip.map(
      s => NSRAInterface(
        nsra = s._1._1._1,
        id = s._2,
        partitionAttribute = s._1._1._2,
        window = s._1._2._1,
        windowTypeStr = s._1._2._2
      )
    )
    fsm
  }

  /**
   * Just deserializes a list of serialized NSRA.
   *
   * @param fn The path to the file/directory containing the serialized list of NSRA.
   *           If file, directly deserialize. If directory, gather all files, deserialize them all and give new
   *           identifiers to avoid id conflicts.
   * @return A list of NSRA
   */
  private def deserialize(fn: String): List[NSRAInterface] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val sdfas = files.flatMap(f => deserializeFile(fn + "/" + f))
      resetIds(sdfas)
    } else throw new Error("Something wrong with given NSRAs file/directory.")
  }

  /**
   * Actual deserialization from a single file.
   *
   * @param fn The path to the file containing the serialized list of NSRA.
   * @return   A list of NSRA.
   */
  private def deserializeFile(fn: String): List[NSRAInterface] = {
    val oisNSRA = new ObjectInputStream(new FileInputStream(fn))
    val nsra = oisNSRA.readObject.asInstanceOf[List[NSRAInterface]]
    oisNSRA.close()
    nsra
  }

  /**
   * Discards the ids of a list of DSRA interfaces and creates new ones.
   *
   * @param dsras The list of the DSRA interfaces.
   * @return      The list of the DSRA interfaces, with new ids.
   */
  private def resetIds(dsras: List[NSRAInterface]): List[NSRAInterface] = {
    dsras.zipWithIndex.map(
      si => NSRAInterface(
        nsra = si._1.nsra,
        id = si._2,
        partitionAttribute = si._1.partitionAttribute,
        window = si._1.window
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
