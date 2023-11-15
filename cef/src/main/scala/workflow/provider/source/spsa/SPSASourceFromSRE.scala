package workflow.provider.source.spsa

import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource

object SPSASourceFromSRE {
  def apply(
             patternFile: String,
             declarationsFile: String,
             streamSource: StreamSource,
             policy: CountPolicy,
             maxNoStates: Int
           ): SPSASourceFromSRE = new SPSASourceFromSRE(patternFile, declarationsFile, streamSource, policy, maxNoStates)
}

class SPSASourceFromSRE(
                         val patternFile: String,
                         val declarationsFile: String,
                         val streamSource: StreamSource,
                         val policy: CountPolicy,
                         val maxNoStates: Int
                       ) extends SPSASource {

}
