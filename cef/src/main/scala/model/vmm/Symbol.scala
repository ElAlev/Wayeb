package model.vmm

object Symbol {
  /**
    * Constructor for symbols.
    *
    * @param value The symbol's value. Must be non-negative.
    * @return The symbol.
    */
  def apply(value: Int): Symbol = {
    require(value >= 0)
    new Symbol(value)
  }

  /**
    * Constructor for the empty symbol. The value of the empty symbol is -1.
    *
    * @return The empty symbol
    */
  def apply(): Symbol = new Symbol(-1) // for the empty symbol
}

/**
  * Class representing symbols in VMMs. For convenience, each symbol is actually represented by an int.
  *
  * @param value The number of the symbol. Symbols with the same value are assumed to be equal.
  */
class Symbol private (val value: Int) extends Serializable with Ordered[Symbol] {

  /**
    * Determines whether this symbol is equal to another, by comparing their values.
    *
    * @param other The other symbol.
    * @return True if both symbols have the same value. If the other object is not a symbol, returns false.
    */
  override def equals(other: Any): Boolean = {
    other match {
      case other: Symbol => other.canEqual(this) && this.value == other.value
      case _ => false
    }
  }

  /**
    * @return The value's hash code.
    */
  override def hashCode(): Int = value.hashCode()

  /**
    * Determines whether we can compare this symbol to another object.
    *
    * @param other The other object.
    * @return True if the other object is also an instance of Symbol.
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Symbol]

  /**
    * Compares this symbol to another, returns their numerical difference.
    *
    * @param other The other symbol.
    * @return The numerical difference of this symbol's value to that of the other's.
    */
  override def compare(other: Symbol): Int = this.value - other.value

  override def toString: String = value.toString
}
