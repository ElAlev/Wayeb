package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.SPSAInterface
import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource
import workflow.provider.SPSAProvider
import workflow.provider.source.spsa.SPSASourceFromSRE
import workflow.task.Task

object SPSATask {
  /**
    * Constructor for SPSA task.
    *
    * @param fn The path to the file with the pattern(s).
    * @param declarations The path to the file with the declarations.
    * @param policy The counting policy.
    * @param streamSource The source for the training stream.
    * @param maxNoStates Tha maximum number allowed for the states of the PSA.
    * @return A SPSA task.
    */
  def apply(
             fn: String,
             declarations: String,
             policy: CountPolicy,
             streamSource: StreamSource,
             maxNoStates: Int
           ): SPSATask = new SPSATask(fn, declarations, policy, streamSource, maxNoStates)

}

/**
  * Creates SPSA(s) from a file with pattern(s) and a training stream.
  *
  * @param fn The path to the file with the pattern(s).
  * @param declarations The path to the file with the declarations.
  * @param policy The counting policy.
  * @param streamSource The source for the training stream.
  * @param maxNoStates Tha maximum number allowed for the states of the PSA.
  */
class SPSATask(
                fn: String,
                declarations: String,
                policy: CountPolicy,
                streamSource: StreamSource,
                maxNoStates: Int
              ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of SPSA (interfaces), one for each pattern.
    */
  override def execute(): List[SPSAInterface] = {
    logger.info("Executing SPSA task...")
    val spsap = SPSAProvider(new SPSASourceFromSRE(fn, declarations, streamSource, policy, maxNoStates))
    val spsa = spsap.provide()
    logger.debug("SPSAs learnt.")
    spsa
  }

}
