package workflow.provider.source.spstm

object SPSTmSourceSerialized {
  def apply(fn: String): SPSTmSourceSerialized = new SPSTmSourceSerialized(fn)
}

class SPSTmSourceSerialized(val fn: String) extends SPSTmSource {

}
