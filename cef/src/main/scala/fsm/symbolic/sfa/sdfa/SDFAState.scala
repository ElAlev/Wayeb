package fsm.symbolic.sfa.sdfa

import fsm.symbolic.sfa.SFAState

case class SDFAState private[sfa] (override val id: Int) extends SFAState(id) {

}
