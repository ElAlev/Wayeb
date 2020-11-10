package fsm

object CountPolicy extends Enumeration {
  type CountPolicy = Value
  val OVERLAP, NONOVERLAP = Value
  def str2Pol(string: String): CountPolicy = string match {
    case "overlap" => OVERLAP
    case "nonoverlap" => NONOVERLAP
    case _ => throw new IllegalArgumentException("Unrecognized policy: " + string)
  }
}
