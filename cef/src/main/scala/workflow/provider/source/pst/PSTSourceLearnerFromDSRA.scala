package workflow.provider.source.pst

import stream.source.StreamSource
import workflow.provider.DSRAProvider

object PSTSourceLearnerFromDSRA {
  def apply(
             dsrap: DSRAProvider,
             trainStream: StreamSource,
             maxOrder: Int,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): PSTSourceLearnerFromDSRA = new PSTSourceLearnerFromDSRA(dsrap, trainStream, maxOrder, pMin, alpha, gammaMin, r)
}

class PSTSourceLearnerFromDSRA(
                                val dsrap: DSRAProvider,
                                val trainStream: StreamSource,
                                val maxOrder: Int,
                                val pMin: Double,
                                val alpha: Double,
                                val gammaMin: Double,
                                val r: Double
                              ) extends PSTSource {

}

