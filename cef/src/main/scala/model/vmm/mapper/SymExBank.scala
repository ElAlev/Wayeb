package model.vmm.mapper

import fsm.symbolic.sra.dsra.{DSRAStreaming, DSRASymbolized}
import model.vmm.Symbol
import stream.GenericEvent
import ui.ConfigUtils

object SymExBank {
  def apply(
             dsra: DSRASymbolized,
             partitionAttribute: String
           ): SymExBank = new SymExBank(dsra, partitionAttribute)

  def apply(dsra: DSRASymbolized): SymExBank = new SymExBank(dsra, ConfigUtils.singlePartitionVal)

  def apply(
             dsraStreaming: DSRAStreaming,
             partitionAttribute: String
           ): SymExBank = {
    val dsraSym = DSRASymbolized(dsraStreaming)
    new SymExBank(dsraSym, partitionAttribute)
  }
}

/**
 * Class representing a bank of symbol extractors for dSRA.
 * Each symbol extractor corresponds to a different value of the partition attribute.
 *
 * @param dsra The symbolized dSRA.
 * @param partitionAttribute The partition attribute.
 */
class SymExBank(
                 dsra: DSRASymbolized,
                 partitionAttribute: String
               ) extends SymbolMapper with Serializable {

  private var symExBank: Map[String, SymbolExtractorFromDSRA] = Map.empty

  /**
   * Finds the symbol corresponding to the new event. First uses the partition attribute in order to find the
   * appropriate extractor.
   *
   * @param event The new event.
   * @return The mapped symbol.
   */
  override def evaluate(event: GenericEvent): Symbol = {
    val partitionId = {
      if (partitionAttribute.equalsIgnoreCase(ConfigUtils.singlePartitionVal)) partitionAttribute
      else event.getValueOf(partitionAttribute).toString
    }
    val symbol = {
      if (symExBank.contains(partitionId)) {
        val thisExtractor = symExBank(partitionId)
        thisExtractor.evaluate(event)
      }
      else {
        val newExtractor = SymbolExtractorFromDSRA(dsra)
        symExBank += (partitionId -> newExtractor)
        newExtractor.evaluate(event)
      }
    }
    symbol
  }

  override def getSymbols: List[Symbol] = dsra.getSymbols

  def getSymbolExtractor: SymbolExtractorFromDSRA = SymbolExtractorFromDSRA(dsra)

}
