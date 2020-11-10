package workflow.task.fsmTask

import com.typesafe.scalalogging.LazyLogging
import fsm.DFAInterface
import fsm.CountPolicy.CountPolicy
import workflow.provider.DFAProvider
import workflow.provider.source.dfa.DFASourceFromXML
import workflow.task.Task

object DFATask {
  /**
    * Constructor for DFA task.
    *
    * @param fn The path to the file.
    * @param policy The counting policy (same for all patterns, if there are multiple).
    * @param order The disambiguation order (same for all patterns, if there are multiple).
    * @param streamSymbols The set of all symbols (same for all patterns, if there are multiple).
    * @return DFA task.
    */
  def apply(
             fn: String,
             policy: CountPolicy,
             order: Int,
             streamSymbols: Set[String]
           ): DFATask = new DFATask(fn, policy, order, streamSymbols)
}

/**
  * Task for creating DFA(s) from a XML file.
  *
  * @param fn The path to the file.
  * @param policy The counting policy (same for all patterns, if there are multiple).
  * @param order The disambiguation order (same for all patterns, if there are multiple).
  * @param streamSymbols The set of all symbols (same for all patterns, if there are multiple).
  */
class DFATask private (
                        fn: String,
                        policy: CountPolicy,
                        order: Int,
                        streamSymbols: Set[String]
                      ) extends Task with LazyLogging {

  /**
    * Executes the task.
    *
    * @return A list of DFAs (interfaces), one for each pattern in the file.
    */
  override def execute(): List[DFAInterface] = {
    logger.info("Executing DFA task...")
    val dfap = DFAProvider(DFASourceFromXML(fn, policy, order, streamSymbols))
    val dfas = dfap.provide()
    logger.info("\t\t\tDFAs:\n" + dfas.toString)
    logger.info("done.")
    dfas
  }

}
