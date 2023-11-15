package model.vmm.mapper

import fsm.symbolic.sra.Configuration
import fsm.symbolic.sra.dsra.DSRASymbolized
import model.vmm.Symbol
import stream.GenericEvent


object SymbolExtractorFromDSRA {
  def apply(dsra: DSRASymbolized): SymbolExtractorFromDSRA = new SymbolExtractorFromDSRA(dsra)
}

/**
 * Symbol mapper for symbolic automata with registers.
 * A symbolized dSRA must be given as argument.
 * A symbol is mapped to the transition that is triggered by an event, taking into account the contents of the
 * registers.
 *
 * @param dsra The symbolized dSRA.
 */
class SymbolExtractorFromDSRA(val dsra: DSRASymbolized) extends SymbolMapper with Serializable {

  private var conf = Configuration(dsra.start)

  override def evaluate(event: GenericEvent): Symbol = {
    conf = dsra.yieldsSuccessorConfig(conf, event).head
    conf.symbol
  }

  override def getSymbols: List[Symbol] = dsra.getSymbols

}
