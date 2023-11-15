package workflow.provider.source.dfa

import fsm.symbolic.sfa.sdfa.SDFA
import model.vmm.mapper.Isomorphism

object DFASourceFromSDFA {
  def apply(
             sdfa: SDFA,
             iso: Isomorphism
           ): DFASourceFromSDFA = new DFASourceFromSDFA(sdfa, iso)
}

class DFASourceFromSDFA(
                         val sdfa: SDFA,
                         val iso: Isomorphism
                       ) extends DFASource {

}
