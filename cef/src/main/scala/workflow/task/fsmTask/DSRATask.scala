package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.DSRAInterface
import workflow.provider.DSRAProvider
import workflow.provider.source.dsra.DSRASourceFromSREM
import workflow.task.Task

object DSRATask {
  /**
   * Constructor for DSRA task.
   *
   * @param fn           Path to file containing patterns
   * @param declarations Path to file containing declarations, i.e., extras (extra predicates to be taken into account)
   *                     and exclusives, i.e., sets of mutually exclusive predicates
   * @return The DSRA task.
   */
  def apply(
             fn: String,
             declarations: String
           ): DSRATask = new DSRATask(fn, declarations)
}

/**
 * Builds DSRA provider from a set of patterns.
 *
 * @param fn Path to file containing patterns
 * @param declarations Path to file containing declarations, i.e., extras (extra predicates to be taken into account)
 *                     and exclusives, i.e., sets of mutually exclusive predicates
 */
class DSRATask private (
                         fn: String,
                         declarations: String
                       ) extends Task with LazyLogging {

  /**
   * Executes the task.
   *
   * @return A list of DSRA(s) (interfaces), one for each pattern.
   */
  override def execute(): List[DSRAInterface] = {
    logger.info("Executing DSRA task...")
    val dsrap = DSRAProvider(new DSRASourceFromSREM(fn, declarations))
    val dsra = dsrap.provide()
    logger.debug("DSRAs built.")
    logger.info("DSRA task done.")
    dsra
  }
}
