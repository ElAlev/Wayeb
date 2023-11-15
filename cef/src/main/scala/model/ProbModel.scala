package model

object ProbModel extends Enumeration {
  type ProbModel = Value
  val FMM, VMM = Value

  def string2ProbModel(str: String): ProbModel = {
    str match {
      case "fmm" => FMM
      case "vmm" => VMM
      case _ => throw new IllegalArgumentException("Probabilistic model not recognized " + str)
    }
  }

}
