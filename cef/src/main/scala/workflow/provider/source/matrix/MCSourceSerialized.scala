package workflow.provider.source.matrix

object MCSourceSerialized {
  def apply(fn: String): MCSourceSerialized = new MCSourceSerialized(fn)
}
class MCSourceSerialized(val fn: String) extends MatrixSource {

}
