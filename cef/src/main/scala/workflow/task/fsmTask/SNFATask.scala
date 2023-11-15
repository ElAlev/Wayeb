package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.SNFAInterface
import workflow.provider.SNFAProvider
import workflow.provider.source.snfa.SNFASourceFromSRE
import workflow.task.Task

object SNFATask {
  /**
   * Constructor for SNFA task.
   *
   * @param fn Path to file containing patterns
   * @return The SNFA task.
   */
  def apply(fn: String): SNFATask = new SNFATask(fn)
}

/**
 * Builds SNFA provider from a set of patterns.
 *
 * @param fn Path to file containing patterns
 */
class SNFATask private (fn: String) extends Task with LazyLogging {

  override def execute(): List[SNFAInterface] = {
    logger.info("Executing SNFA task...")
    val snfap = SNFAProvider(new SNFASourceFromSRE(fn))
    val snfa = snfap.provide()
    logger.debug("SNFAs built.")
    logger.info("SNFA task done.")
    snfa
  }

}
