package fsm

object FSMModel extends Enumeration {
  type FSMModel = Value
  val NSFA, DSFA, NSRA, DSRA = Value

  def string2FSMModel(str: String): FSMModel = {
    str match {
      case "nsfa" => NSFA
      case "dsfa" => DSFA
      case "nsra" => NSRA
      case "dsra" => DSRA
      case _ => throw new IllegalArgumentException("FSM model not recognized " + str)
    }
  }
}
