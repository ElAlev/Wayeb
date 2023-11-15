package workflow.task.fsmTask
import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy.CountPolicy
import fsm.SPSTInterface
import stream.source.StreamSource
import workflow.provider.SPSTProvider
import workflow.provider.source.spst.SPSTSourceFromSRE
import workflow.task.Task

object SPSTTask {

  /**
    * Constructor for SPST task.
    *
    * @param fn The path to the file with the pattern(s).
    * @param declarations The path to the file with the declarations.
    * @param policy The counting policy.
    * @param streamSource The source for the training stream.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded (same for all SPST).
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold (same for all SPST).
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold (same for all SPST).
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold (same
    *          for all SPST).
    * @return A SPST task.
    */
  def apply(
             fn: String,
             declarations: String,
             policy: CountPolicy,
             streamSource: StreamSource,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): SPSTTask =
    new SPSTTask(fn, declarations, policy, streamSource, pMin, alpha, gammaMin, r)
}

/**
  * Creates SPST(s) from a file with pattern(s) and a training stream.
  *
  * @param fn The path to the file with the pattern(s).
  * @param declarations The path to the file with the declarations.
  * @param policy The counting policy.
  * @param streamSource The source for the training stream.
  * @param pMin This is the symbol threshold. Symbols with lower probability are discarded (same for all SPST).
  * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
  *              context must be greater than this threshold (same for all SPST).
  * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
  *                 expanded context must be greater than this threshold (same for all SPST).
  * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
  *          on the expanded context by the conditional on the original context is greater than this threshold (same
  *          for all SPST).
  */
class SPSTTask(
                fn: String,
                declarations: String,
                policy: CountPolicy,
                streamSource: StreamSource,
                pMin: Double,
                alpha: Double,
                gammaMin: Double,
                r: Double
              ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of SPST (interfaces), one for each pattern.
    */
  override def execute(): List[SPSTInterface] = {
    logger.info("Executing SPST task...")
    val spstp = SPSTProvider(new SPSTSourceFromSRE(fn,declarations,streamSource,policy,pMin,alpha,gammaMin,r))
    val spst = spstp.provide()
    logger.debug("SPSTs learnt.")
    spst
  }
}
