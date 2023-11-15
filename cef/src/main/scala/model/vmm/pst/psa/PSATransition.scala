package model.vmm.pst.psa

import model.vmm.Symbol

object PSATransition {
  /**
    * Constructor for PSA transitions.
    *
    * @param target The target state.
    * @param symbol The transition's symbol.
    * @param prob The transition;s probability.
    * @return A PSA transition.
    */
  def apply(
             target: PSAState,
             symbol: Symbol, prob: Double
           ): PSATransition = new PSATransition(target, symbol, prob)
}

/**
  * Outgoing transition of PSAs.
  *
  * @param target The target state.
  * @param symbol The transition's symbol.
  * @param prob The transition;s probability.
  */
class PSATransition(
                     val target: PSAState,
                     val symbol: Symbol,
                     val prob: Double
                   ) extends Serializable {

  override def toString: String = "Target: " + target.label + " with " + symbol + " and probability " + prob
}
