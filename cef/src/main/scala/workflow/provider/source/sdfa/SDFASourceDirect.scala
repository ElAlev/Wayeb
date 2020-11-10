package workflow.provider.source.sdfa

import fsm.symbolic.sfa.sdfa.SDFA

object SDFASourceDirect {
  def apply(
             sdfa: List[SDFA],
             partitionAttributes: List[String]
           ): SDFASourceDirect = new SDFASourceDirect(sdfa, partitionAttributes)

  def apply(sdfa: List[SDFA]): SDFASourceDirect = new SDFASourceDirect(sdfa, List.empty)
}

class SDFASourceDirect(
                        val sdfa: List[SDFA],
                        val partitionAttributes: List[String]
                      ) extends SDFASource {
  require(partitionAttributes.isEmpty | (sdfa.size == partitionAttributes.size))
}
