package workflow.provider.source.snfa

object SNFASourceSerialized {
  def apply(fn: String): SNFASourceSerialized = new SNFASourceSerialized(fn)
}

class SNFASourceSerialized(val fn: String) extends SNFASource {

}
