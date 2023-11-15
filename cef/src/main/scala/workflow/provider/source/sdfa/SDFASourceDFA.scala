package workflow.provider.source.sdfa

import fsm.classical.fa.dfa.DFA
import model.vmm.mapper.Isomorphism

object SDFASourceDFA {
  def apply(
             dfa: DFA,
             iso: Isomorphism
           ): SDFASourceDFA = new SDFASourceDFA(dfa, iso)

}

class SDFASourceDFA(
                     val dfa: DFA,
                     val iso: Isomorphism
                   ) extends SDFASource {

}
