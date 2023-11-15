package workflow.provider.source.snfa

object SNFASourceFromSRE {
  def apply(sreFile: String): SNFASourceFromSRE = new SNFASourceFromSRE(sreFile)
}

class SNFASourceFromSRE(val sreFile: String) extends SNFASource {

}
