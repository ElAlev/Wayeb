package workflow.provider.source.dsra

object DSRASourceFromSREM {
  def apply(
             sreFile: String,
             declarations: String
           ): DSRASourceFromSREM = new DSRASourceFromSREM(sreFile, declarations)
}

class DSRASourceFromSREM(
                          val sreFile: String,
                          val declarations: String
                        ) extends DSRASource {

}
