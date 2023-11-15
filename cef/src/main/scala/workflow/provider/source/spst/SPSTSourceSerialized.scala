package workflow.provider.source.spst

object SPSTSourceSerialized {
  def apply(fn: String): SPSTSourceSerialized = new SPSTSourceSerialized(fn)
}

class SPSTSourceSerialized(val fn: String) extends SPSTSource {

}
