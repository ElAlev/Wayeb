package model.vmm.pst.spsa

import model.vmm.Symbol

object SPSATransition {
  /**
    * Constructor for SPSA transitions.
    *
    * @param target The target state.
    * @param withSymbol The transition symbol.
    * @param prob The transition probability.
    * @return A SPSA transition.
    */
  def apply(
             target: SPSAState,
             withSymbol: Symbol,
             prob: Double
           ): SPSATransition = new SPSATransition(target, withSymbol, prob)
}

/**
  * Represents a SPSA transition. Each transition has a symbol and a probability.
  * The source state is not given, because transitions are always created from within a SPSA state and the source
  * state is thus implicitly known.
  * As target state, we directly pass the object of that state and not the target state's id.
  * CAUTION: this might create problems during serialization due to infinite recursion.
  *
  * TODO: maybe target should be the state's identifier and not the state itself due to overflows during serialization
  *
  * @param target The target state.
  * @param withSymbol The transition symbol.
  * @param prob The transition probability.
  */
class SPSATransition private[spsa] (
                                     val target: SPSAState,
                                     val withSymbol: Symbol,
                                     val prob: Double
                                   ) extends Serializable {

  override def toString: String =
    "SYMBOL: " + withSymbol +
      "\tTARGET: (" + target.sfaId + "->" + target.psaLabel + ")" +
      "\tPROB: " + prob

}
