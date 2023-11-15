package workflow.provider.source.dsra

import fsm.symbolic.sra.dsra.DSRAStreaming

object DSRASourceDirect {
  def apply(
             dsra: List[DSRAStreaming],
             partitionAttributes: List[String]
           ): DSRASourceDirect = new DSRASourceDirect(dsra, partitionAttributes)

  def apply(dsra: List[DSRAStreaming]): DSRASourceDirect = new DSRASourceDirect(dsra, List.empty)
}

class DSRASourceDirect (
                         val dsra: List[DSRAStreaming],
                         val partitionAttributes: List[String]
                       ) extends DSRASource {
  require(partitionAttributes.isEmpty | (dsra.size == partitionAttributes.size))

}
