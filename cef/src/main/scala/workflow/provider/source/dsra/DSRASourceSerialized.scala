package workflow.provider.source.dsra

object DSRASourceSerialized {
  def apply(fn: String): DSRASourceSerialized = new DSRASourceSerialized(fn)
}

class DSRASourceSerialized(val fn: String) extends DSRASource {

}
