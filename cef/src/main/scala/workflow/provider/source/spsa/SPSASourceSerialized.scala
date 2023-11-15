package workflow.provider.source.spsa

object SPSASourceSerialized {
  def apply(fn: String): SPSASourceSerialized = new SPSASourceSerialized(fn)
}

class SPSASourceSerialized(val fn: String) extends SPSASource {

}
