package workflow.provider.source.pst

import stream.source.StreamSource
import workflow.provider.SDFAProvider

object PSTSourceLearnerFromSDFA {
  def apply(
             sdfap: SDFAProvider,
             trainStream: StreamSource,
             maxOrder: Int,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): PSTSourceLearnerFromSDFA = new PSTSourceLearnerFromSDFA(sdfap, trainStream, maxOrder, pMin, alpha, gammaMin, r)
}

class PSTSourceLearnerFromSDFA(
                                val sdfap: SDFAProvider,
                                val trainStream: StreamSource,
                                val maxOrder: Int,
                                val pMin: Double,
                                val alpha: Double,
                                val gammaMin: Double,
                                val r: Double
                              ) extends PSTSource {

}
