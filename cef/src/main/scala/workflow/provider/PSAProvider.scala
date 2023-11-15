package workflow.provider

import java.io.{FileInputStream, ObjectInputStream}
import com.typesafe.scalalogging.LazyLogging
import model.vmm.pst.psa.ProbSuffixAutomaton
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.psa.{PSASource, PSASourceDirect, PSASourceSerialized}

object PSAProvider {

  /**
    * Constructor for PSA provider when psaSource is workflow.provider.source.psa.PSASourceDirect.
    * The source contains already the PSA.
    *
    * @param psaSource A workflow.provider.source.psa.PSASourceDirect.
    * @return A PSA provider.
    */
  def apply(psaSource: PSASourceDirect): PSAProvider = new PSAProvider(psaSource, List.empty)

  /**
    * Constructor for PSA provider when psaSource is workflow.provider.source.psa.PSASourceSerialized.
    * The source contains already the path to the file with the serialized PSA.
    *
    * @param psaSource A workflow.provider.source.psa.PSASourceSerialized.
    * @return A PSA provider.
    */
  def apply(psaSource: PSASourceSerialized): PSAProvider =
    new PSAProvider(psaSource, List(new FileExistsCondition(psaSource.fn)))
}

/**
  * According to type of source for PSA, builds a provider in the form of a list of PSAs.
  *
  * @param psaSource The source for the PSA:
  *                  - PSASourceDirect when a PSA already exists.
  *                  - PSASourceSerialized when PSA have been serialized and stored.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class PSAProvider private (
                            psaSource: PSASource,
                            conditions: List[Condition]
                          ) extends AbstractProvider(conditions) with LazyLogging {
  checkConditions()

  /**
    * Calling this function actually initiates the construction of the PSA.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the path to the file with the serialized PSA) and
    * then call provide(). Keep the source until you call provide().
    *
    * @return A list of PSA.
    */
  override def provide(): List[ProbSuffixAutomaton] = {
    psaSource match {
      case x: PSASourceDirect => x.psa
      case x: PSASourceSerialized => deserializeFile(x.fn)
      case _ => {
        logger.error("Not valid PSASource")
        throw new Error("Not valid PSASource")
      }
    }
  }

  /**
    * deserialization from a single file.
    *
    * @param fn The path to the file containing the serialized list of PSA.
    * @return A list of PSA
    */
  private def deserializeFile(fn: String): List[ProbSuffixAutomaton] = {
    val oisPSA = new ObjectInputStream(new FileInputStream(fn))
    val psa = oisPSA.readObject.asInstanceOf[List[ProbSuffixAutomaton]]
    oisPSA.close()
    psa
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
