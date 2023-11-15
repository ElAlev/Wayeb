package fsm.symbolic.sra

import fsm.symbolic.TransitionOutput.{TAKE, TransitionOutput}
import fsm.symbolic.Valuation
import model.vmm.Symbol

object Configuration {
  def apply(stateId: Int): Configuration = new Configuration(stateId, Valuation(), 0, Symbol(), TAKE)

  def apply(
             stateId: Int,
             valuation: Valuation
           ): Configuration = new Configuration(stateId, valuation, 0, Symbol(), TAKE)

  def apply(
             stateId: Int,
             valuation: Valuation,
             output: TransitionOutput
           ): Configuration = new Configuration(stateId, valuation, 0, Symbol(), output)

  def apply(
             stateId: Int,
             valuation: Valuation,
             index: Int,
             symbol: Symbol
           ): Configuration = new Configuration(stateId, valuation, index, symbol, TAKE)

  def apply(
             stateId: Int,
             valuation: Valuation,
             index: Int,
             symbol: Symbol,
             output: TransitionOutput
           ): Configuration = new Configuration(stateId, valuation, index, symbol, output)

  def apply(
             stateId: Int,
             valuation: Valuation,
             index: Int
           ): Configuration = new Configuration(stateId, valuation, index, Symbol(), TAKE)

  def apply(
             stateId: Int,
             valuation: Valuation,
             index: Int,
             output: TransitionOutput
           ): Configuration = new Configuration(stateId, valuation, index, Symbol(), output)

  def apply(
             stateId: Int,
             output: TransitionOutput
           ): Configuration = new Configuration(stateId, Valuation(), 0, Symbol(), output)
}

/**
 * Class representing configurations of symbolic register automata.
 * A configuration is composed of
 * 1) the automaton's current state
 * 2) the current valuation, i.e., the contents of the automaton's register
 * 3) the last "symbol" emitted by the automaton. Strictly speaking, this symbol is not really a part of a configuration.
 *    It is included, however, because it helps in forecasting. In forecasting, each SRA transition corresponds to a
 *    symbol. When the SRA moves to a new configuration after it has read an event, we need to know which symbol it
 *    "emitted" (which transition was triggered), because we need those symbols in order to use counter/prediction
 *    suffix trees.
 * 4) the last output emitted by the automaton, i.e., the output of the last triggered transition that brought the
 *    automaton to its current state. Strictly speaking, the output is not really a part of a configuration.
 *    Included here for convenience.
 *
 * @param stateId The id of the current state.
 * @param valuation The current valuation.
 * @param symbol The last symbol "emitted" by the SRA.
 * @param output The last output "emitted" by the SRA.
 */
class Configuration(
                     val stateId: Int,
                     val valuation: Valuation,
                     val index: Int,
                     val symbol: Symbol,
                     val output: TransitionOutput
                   ) extends Serializable {
  def getRegisterNames: Set[String] = valuation.getRegisterNames

  override def toString: String = stateId.toString + "|" + valuation.toString

}
