package workflow.provider.source.sdfa

object SDFASourceSerialized {
  def apply(fn: String): SDFASourceSerialized = new SDFASourceSerialized(fn)
}

class SDFASourceSerialized(val fn: String) extends SDFASource {

}
