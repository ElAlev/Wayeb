package workflow.provider.source.psa

object PSASourceSerialized {
  def apply(fn: String): PSASourceSerialized = new PSASourceSerialized(fn)
}

class PSASourceSerialized(val fn: String) extends PSASource {

}
