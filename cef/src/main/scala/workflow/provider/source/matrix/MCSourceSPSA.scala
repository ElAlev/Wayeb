package workflow.provider.source.matrix

import workflow.provider.{FSMProvider, SPSAProvider}

object MCSourceSPSA {
  def apply(fsmp: FSMProvider): MCSourceSPSA = {
    require(fsmp.isSPSA)
    new MCSourceSPSA(fsmp.wrappedProvider.asInstanceOf[SPSAProvider])
  }
}

class MCSourceSPSA(val spsa: SPSAProvider) extends MatrixSource {

}
