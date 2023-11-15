package fsm.symbolic.sfa.snfa

import fsm.symbolic.sfa.SFAState

case class SNFAState private[snfa](override val id: Int) extends SFAState(id = id) {

}
