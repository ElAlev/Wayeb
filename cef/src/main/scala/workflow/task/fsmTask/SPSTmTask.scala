package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.SPSTmInterface
import stream.source.StreamSource
import workflow.provider.SPSTmProvider
import workflow.provider.source.spstm.SPSTmSourceFromSREM
import workflow.task.Task

object SPSTmTask {
  /**
   * Constructor for SPSTm task.
   *
   * @param fn           The path to the file with the pattern(s).
   * @param declarations The path to the file with the declarations.
   * @param streamSource The source for the training stream.
   * @param pMin         This is the symbol threshold. Symbols with lower probability are discarded (same for all SPSTm).
   * @param alpha        Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
   *                     context must be greater than this threshold (same for all SPSTm).
   * @param gammaMin     Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
   *                     expanded context must be greater than this threshold (same for all SPSTm).
   * @param r            This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
   *                     on the expanded context by the conditional on the original context is greater than this threshold (same
   *                     for all SPSTm).
   * @return The SPSTm task.
   */
  def apply(
             fn: String,
             declarations: String,
             streamSource: StreamSource,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): SPSTmTask = new SPSTmTask(fn, declarations, streamSource, pMin, alpha, gammaMin, r)
}

/**
 * Creates SPSTm(s) from a file with pattern(s) and a training stream.
 *
 * @param fn The path to the file with the pattern(s).
 * @param declarations The path to the file with the declarations.
 * @param streamSource The source for the training stream.
 * @param pMin This is the symbol threshold. Symbols with lower probability are discarded (same for all SPSTm).
 * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
 *              context must be greater than this threshold (same for all SPSTm).
 * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
 *                 expanded context must be greater than this threshold (same for all SPSTm).
 * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
 *          on the expanded context by the conditional on the original context is greater than this threshold (same
 *          for all SPSTm).
 */
class SPSTmTask (
                  fn: String,
                  declarations: String,
                  streamSource: StreamSource,
                  pMin: Double,
                  alpha: Double,
                  gammaMin: Double,
                  r: Double
                ) extends Task with LazyLogging {
  /**
   * Executes the task.
   *
   * @return A list of SPSTm (interfaces), one for each pattern.
   */
  override def execute(): List[SPSTmInterface] = {
    logger.info("Executing SPSTm task...")
    val spstmp = SPSTmProvider(new SPSTmSourceFromSREM(fn,declarations,streamSource,pMin,alpha,gammaMin,r))
    val spstm = spstmp.provide()
    logger.debug("SPSTms learnt.")
    spstm
  }

}
