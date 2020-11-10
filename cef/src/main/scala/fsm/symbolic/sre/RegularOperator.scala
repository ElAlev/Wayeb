package fsm.symbolic.sre

object RegularOperator extends Enumeration {
  type RegularOperator = Value
  val SEQ, CHOICE, ITER, NEG, ANY, NEXT = Value
}
