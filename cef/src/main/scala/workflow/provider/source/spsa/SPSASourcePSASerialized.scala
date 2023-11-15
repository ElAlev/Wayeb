package workflow.provider.source.spsa

import fsm.CountPolicy.CountPolicy
import workflow.provider.PSAProvider

object SPSASourcePSASerialized {
  def apply(
             patternFile: String,
             declarationsFile: String,
             psap: PSAProvider,
             policy: CountPolicy
           ): SPSASourcePSASerialized = new SPSASourcePSASerialized(patternFile, declarationsFile, psap, policy)
}

class SPSASourcePSASerialized(
                               val patternFile: String,
                               val declarationsFile: String,
                               val psap: PSAProvider,
                               val policy: CountPolicy
                             ) extends SPSASource {

}
