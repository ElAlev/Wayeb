package workflow.provider.source.spstm

import stream.source.StreamSource

object SPSTmSourceFromSREM {
  def apply(
             patternFile: String,
             declarationsFile: String,
             streamSource: StreamSource,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): SPSTmSourceFromSREM = new SPSTmSourceFromSREM(patternFile, declarationsFile, streamSource, pMin, alpha, gammaMin, r)
}

class SPSTmSourceFromSREM(
                           val patternFile: String,
                           val declarationsFile: String,
                           val streamSource: StreamSource,
                           val pMin: Double,
                           val alpha: Double,
                           val gammaMin: Double,
                           val r: Double
                         ) extends SPSTmSource {

}
