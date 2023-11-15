package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.NSRAInterface
import workflow.provider.NSRAProvider
import workflow.provider.source.nsra.NSRASourceFromSREM
import workflow.task.Task

object NSRATask {
  /**
   * Constructor for NSRA task.
   *
   * @param fn Path to file containing patterns.
   * @return The NSRA task.
   */
  def apply(fn: String): NSRATask = new NSRATask(fn)
}

/**
 * Builds NSRA provider from a set of patterns.
 *
 * @param fn Path to file containing patterns.
 */
class NSRATask private (fn: String) extends Task with LazyLogging {

  override def execute(): List[NSRAInterface] = {
    logger.info("Executing NSRA task...")
    val nsrap = NSRAProvider(new NSRASourceFromSREM(fn))
    val nsra = nsrap.provide()
    logger.debug("NSRAs built.")
    logger.info("NSRA task done.")
    nsra
  }

}
