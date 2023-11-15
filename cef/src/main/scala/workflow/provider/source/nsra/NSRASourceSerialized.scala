package workflow.provider.source.nsra

object NSRASourceSerialized {
  def apply(fn: String): NSRASourceSerialized = new NSRASourceSerialized(fn)
}

class NSRASourceSerialized(val fn: String) extends NSRASource {

}
