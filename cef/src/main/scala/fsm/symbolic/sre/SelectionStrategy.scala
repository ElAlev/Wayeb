package fsm.symbolic.sre

object SelectionStrategy extends Enumeration {
  type SelectionStrategy = Value
  val STRICT, ANY, NEXT = Value
}
