package model.vmm.mapper

import fsm.symbolic.logic.Sentence
import model.vmm.Symbol
import stream.GenericEvent

object Isomorphism {

  /**
    * Constructor for isomorphism.
    *
    * @param minTermsList The list of minterms.
    * @param symbolsList The list of symbols.
    * @return The isomorphism.
    */
  def apply(
             minTermsList: List[Sentence],
             symbolsList: List[Symbol]
           ): Isomorphism = new Isomorphism(minTermsList, symbolsList)

  /**
    * Constructor for isomorphism. Symbols not directly given. A list of increasing integers created as symbols.
    *
    * @param minTerms The list of minterms.
    * @return The isomorphism.
    */
  def apply(minTerms: Set[Sentence]): Isomorphism = {
    val symbolsList = (1 to minTerms.size).map(x => Symbol(x)).toList
    val minTermsList = minTerms.toList
    new Isomorphism(minTermsList, symbolsList)
  }
}

/**
  * Class representing the mapping of an isomorphism from minterms to symbols. Contains only the mapping, not the
  * operator of the isomorphism.
  *
  * @param minTermsList The list of minterms.
  * @param symbolsList The list of symbols.
  */
class Isomorphism(
                   val minTermsList: List[Sentence],
                   val symbolsList: List[Symbol]
                 ) extends SymbolMapper with Serializable {
  private val minTermsArray = minTermsList.toArray
  private val symbolsArray = symbolsList.toArray
  private val m2s = minTermsArray.zip(symbolsArray).toMap
  private val s2m = symbolsArray.zip(minTermsArray).toMap

  /**
    * Retrieves the symbol corresponding to a given minterm.
    * @param minTerm Teh given minterm.
    * @return The corresponding symbol.
    */
  def getSymbolForMinTerm(minTerm: Sentence): Symbol = m2s(minTerm)

  /**
    * Retrieves the minterm corresponding to the given symbol.
    *
    * @param symbol The given symbol.
    * @return The corresponding minterm.
    */
  def getMinTermForSymbol(symbol: Symbol): Sentence = s2m(symbol)

  /**
    * Finds the symbol "triggered" by a given event. First finds which minterm evaluates to true and then retrieves the
    * corresponding symbol. Assumes exactly one minterm evaluates to true.
    *
    * @param event The given event.
    * @return The triggered symbol.
    */
  override def evaluate(event: GenericEvent): Symbol = {
    val i = minTermsArray.map(x => x.evaluate(event)).indexOf(true)
    symbolsArray(i)
  }

  /**
    * @return All the symbols.
    */
  override def getSymbols: List[Symbol] = symbolsArray.toList

  override def toString: String = "min-terms to symbols:\n" + m2s + "\n symbols to min-terms:\n" + s2m

}
