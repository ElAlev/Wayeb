package workflow.provider.source.dfa

object DFASourceSerialized {
  def apply(fn: String): DFASourceSerialized = new DFASourceSerialized(fn)
}

class DFASourceSerialized(val fn: String) extends DFASource {

}
