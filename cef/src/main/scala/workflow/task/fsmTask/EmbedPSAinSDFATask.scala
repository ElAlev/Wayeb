package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.SPSAInterface
import fsm.CountPolicy.CountPolicy
import ui.ConfigUtils
import workflow.provider.source.psa.PSASourceSerialized
import workflow.provider.source.spsa.SPSASourcePSASerialized
import workflow.provider.{PSAProvider, SPSAProvider}
import workflow.task.Task

object EmbedPSAinSDFATask {
  /**
    * Constructor for embedding task.
    *
    * @param fn The path to the file with the SRE patterns.
    * @param declarations The path to the declarations file.
    * @param policy The counting policy.
    * @param psaFile The path to the file with the serialized PSA.
    * @return An embedding task.
    */
  def apply(
             fn: String,
             declarations: String,
             policy: CountPolicy,
             psaFile: String
           ): EmbedPSAinSDFATask = new EmbedPSAinSDFATask(fn, declarations, policy, psaFile)

  /**
    * Constructor for embedding task.
    * Default counting policy.
    *
    * @param fn The path to the file with the SRE patterns.
    * @param declarations The path to the declarations file.
    * @param psaFile The path to the file with the serialized PSA.
    * @return An embedding task.
    */
  def apply(
             fn: String,
             declarations: String,
             psaFile: String
           ): EmbedPSAinSDFATask = new EmbedPSAinSDFATask(fn, declarations, ConfigUtils.defaultPolicy, psaFile)
}

/**
  * Embeds a list of PSA in a list of SDFA.
  * Make sure that the list of patterns in the SRE file corresponds to the list of serialized PSA in the PSA file.
  *
  * @param fn The path to the file with the SRE patterns.
  * @param declarations The path to the declarations file.
  * @param policy The counting policy.
  * @param psaFile The path to the file with the serialized PSA.
  */
class EmbedPSAinSDFATask(
                          fn: String,
                          declarations: String,
                          policy: CountPolicy,
                          psaFile: String
                        ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of SPSA, one for each pattern and PSA.
    */
  override def execute(): List[SPSAInterface] = {
    logger.info("Merging SFA-PSA task...")
    val psap = PSAProvider(new PSASourceSerialized(psaFile))
    val spsap = SPSAProvider(new SPSASourcePSASerialized(fn, declarations, psap, policy))
    val spsa = spsap.provide()
    logger.debug("Merging complete.")
    spsa
  }
}
