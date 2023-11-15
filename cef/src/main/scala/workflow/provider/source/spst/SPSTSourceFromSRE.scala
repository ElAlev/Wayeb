package workflow.provider.source.spst

import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource

object SPSTSourceFromSRE {
  def apply(
             patternFile: String,
             declarationsFile: String,
             streamSource: StreamSource,
             policy: CountPolicy,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): SPSTSourceFromSRE =
    new SPSTSourceFromSRE(patternFile, declarationsFile, streamSource, policy, pMin, alpha, gammaMin, r)
}

class SPSTSourceFromSRE(
                         val patternFile: String,
                         val declarationsFile: String,
                         val streamSource: StreamSource,
                         val policy: CountPolicy,
                         val pMin: Double,
                         val alpha: Double,
                         val gammaMin: Double,
                         val r: Double
                       ) extends SPSTSource {

}
