package fsm.classical.pattern.regexp

object SymbolNode {
  def apply(symbol: String): SymbolNode = new SymbolNode(symbol, None)

  def apply(symbol: String, writeRegister: String): SymbolNode = new SymbolNode(symbol, Option(writeRegister))
}

/**
 * Each leaf of the tree is a terminal symbol.
 *
 * @param symbol The node's symbol, as a string.
 */
case class SymbolNode(
                       symbol: String,
                       writeRegister: Option[String]
                     ) extends RegExpTree {
  override def toString: String = "Symbol:" + symbol + "-WriteReg:" + writeRegister
}
