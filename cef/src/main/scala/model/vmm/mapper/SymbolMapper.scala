package model.vmm.mapper

import model.vmm.Symbol
import stream.GenericEvent

/**
 * The function of this trait is to map events to symbols.
 * For symbolic automata, an isomorphism is used.
 * For symbolic automata with registers, each transition is mapped to a symbol.
 */
trait SymbolMapper {

  def evaluate(event: GenericEvent): Symbol

  def getSymbols: List[Symbol]

}
