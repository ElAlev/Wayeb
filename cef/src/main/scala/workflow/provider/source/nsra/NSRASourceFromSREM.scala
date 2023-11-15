package workflow.provider.source.nsra

object NSRASourceFromSREM {
  def apply(sremFile: String): NSRASourceFromSREM = new NSRASourceFromSREM(sremFile)
}

class NSRASourceFromSREM(val sremFile: String) extends NSRASource {

}
