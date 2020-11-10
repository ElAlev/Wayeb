package fsm.classical

object FATransition {
  /**
    * Constructs a transitions from source to target with symbol.
    *
    * @param source The id of the source state.
    * @param target The id of the target state.
    * @param symbol The symbol.
    * @return A transition.
    */
  def apply(
      source: Int,
      target: Int,
      symbol: String
  ): FATransition = new FATransition(source, target, symbol)
}

/**
  * Class representing transitions for classical finite automata.
  *
  * @param source The id of the source state.
  * @param target The id of the target state.
  * @param symbol The transition symbol.
  */
class FATransition(
                    val source: Int,
                    val target: Int,
                    val symbol: String
                  ) {

  override def toString: String = {
    source + "-->" + target + "\t(" + symbol + ")"
  }

}
