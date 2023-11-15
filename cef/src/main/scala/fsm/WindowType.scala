package fsm

object WindowType extends Enumeration {
  type WindowType = Value
  val COUNT, TIME = Value

  def str2Wt(string: String): WindowType = string match {
    case "count" => COUNT
    case "time" => TIME
    case _ => throw new IllegalArgumentException("Unrecognized window type: " + string)
  }
}
