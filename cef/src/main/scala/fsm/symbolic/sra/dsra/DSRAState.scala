package fsm.symbolic.sra.dsra

import fsm.symbolic.sra.SRAState

case class DSRAState private[dsra] (override val id: Int) extends SRAState(id) {
  
}
