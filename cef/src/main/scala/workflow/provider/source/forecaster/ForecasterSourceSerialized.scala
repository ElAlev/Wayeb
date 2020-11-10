package workflow.provider.source.forecaster

object ForecasterSourceSerialized {
  def apply(fn: String): ForecasterSourceSerialized = new ForecasterSourceSerialized(fn)
}

class ForecasterSourceSerialized(val fn: String) extends ForecasterSource {

}
