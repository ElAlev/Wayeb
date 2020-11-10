package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.SDFAInterface
import fsm.CountPolicy.CountPolicy
import ui.ConfigUtils
import workflow.provider.SDFAProvider
import workflow.provider.source.sdfa.SDFASourceFromSRE
import workflow.task.Task

object SDFATask {
  /**
    * Constructor for SDFA task.
    * Default value for minTermMethod.
    *
    * @param fn Path to file containing patterns
    * @param declarations Path to file containing declarations, i.e., extras (extra predicates to be taken into account)
    *                     and exclusives, i.e., sets of mutually exclusive predicates
    * @param policy Counting policy, overlap or non-overlap.
    * @return A SDFA task.
    */
  def apply(
             fn: String,
             declarations: String,
             policy: CountPolicy
           ): SDFATask = new SDFATask(fn, declarations, policy, ConfigUtils.defaultMinTermMethod)

  /**
    * Constructor for SDFA task.
    * Default value for minTermMethod, policy.
    *
    * @param fn Path to file containing patterns
    * @param declarations Path to file containing declarations, i.e., extras (extra predicates to be taken into account)
    *                     and exclusives, i.e., sets of mutually exclusive predicates
    * @return A SDFA task.
    */
  def apply(
             fn: String,
             declarations: String
           ): SDFATask = new SDFATask(fn, declarations, ConfigUtils.defaultPolicy, ConfigUtils.defaultMinTermMethod)


}

/**
  * Builds SDFA provider from a set of patterns.
  *
  * @param fn Path to file containing patterns
  * @param declarations Path to file containing declarations, i.e., extras (extra predicates to be taken into account)
  *                     and exclusives, i.e., sets of mutually exclusive predicates
  * @param policy Counting policy, overlap or non-overlap.
  * @param minTermMethod The method for building minterms, "withsat" or "withoutsat".
  */

class SDFATask private (
                         fn: String,
                         declarations: String,
                         policy: CountPolicy,
                         minTermMethod: String
                       ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of SDFA(s) (interfaces), one for each pattern.
    */
  override def execute(): List[SDFAInterface] = {
    logger.info("Executing SDFA task...")
    val sdfap = SDFAProvider(new SDFASourceFromSRE(fn, policy, declarations, minTermMethod))
    val sdfa = sdfap.provide()
    logger.debug("SDFAs built.")
    //logger.debug(sdfa.foldLeft("") { (acc,x) => acc + "\t\t\tSDFA:\n" + x.toString})
    logger.info("SDFA task done.")
    sdfa
  }

}
