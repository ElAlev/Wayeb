package fsm.symbolic.sra.dsra

import fsm.symbolic.sra.Configuration
import model.vmm.Symbol
import stream.GenericEvent
import java.io.{FileOutputStream, ObjectOutputStream}

object DSRASymbolized {
  def apply(dsra: DSRAStreaming): DSRASymbolized = new DSRASymbolized(dsra)
}

/**
 * Class representing deterministic symbolic automata, where each state/transition is assigned a symbol.
 * To be used for forecasting with SRA. The symbols created for the states/transitions will be used to construct
 * counter/prediction suffix trees.
 *
 * @param dsra The initial streaming DSRA.
 */
class DSRASymbolized private[dsra] (dsra: DSRAStreaming) {

  val start: Int = dsra.start

  /**
   * Produces a set of new successor configurations from a given configuration and a given input event.
   * The returned set should be a singleton.
   *
   * @param fromConfiguration The given configuration.
   * @param withEvent         The given event.
   * @return                  A set of new successor configurations (should be a singleton).
   */
  def yieldsSuccessorConfig(
                             fromConfiguration: Configuration,
                             withEvent: GenericEvent
                           ): Set[Configuration] = dsra.yieldsSuccessorConfig(fromConfiguration, withEvent)


  /**
   * Finds the state we can reach from another given state and a given symbol.
   *
   * @param state   The given state.
   * @param symbol  The given symbol. Must belong to the sets of symbols of the automaton.
   * @return        The id of the next state. -1 if no next can be found.
   */
  def getNextStateWithSymbol(
                              state: Int,
                              symbol: Symbol
                            ): Int = dsra.getNextStateWithSymbol(state, symbol)

  /**
   * @return All the hyper-symbols of the DSRA. CAUTION: hyper-symbols correspond to hyper-states.
   */
  def getSymbols: List[Symbol] = dsra.hyperStates.map(s => Symbol(s))

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
