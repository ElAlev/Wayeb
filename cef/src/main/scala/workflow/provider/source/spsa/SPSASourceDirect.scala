package workflow.provider.source.spsa

import model.vmm.pst.spsa.SymbolicPSA

object SPSASourceDirect {
  def apply(
             spsa: List[SymbolicPSA],
             partitionAttributes: List[String]
           ): SPSASourceDirect = new SPSASourceDirect(spsa, partitionAttributes)

  def apply(spsa: List[SymbolicPSA]): SPSASourceDirect = new SPSASourceDirect(spsa, List.empty)
}

class SPSASourceDirect(
                        val spsa: List[SymbolicPSA],
                        val partitionAttributes: List[String]
                      ) extends SPSASource {
  require(partitionAttributes.isEmpty | (spsa.size == partitionAttributes.size))
}
