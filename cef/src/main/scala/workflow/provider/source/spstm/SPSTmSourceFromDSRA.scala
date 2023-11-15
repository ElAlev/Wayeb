package workflow.provider.source.spstm

import stream.source.StreamSource
import workflow.provider.DSRAProvider
import workflow.provider.source.dsra.DSRASourceDirectI
import workflow.provider.source.pst.{PSTSource, PSTSourceLearnerFromDSRA}

object SPSTmSourceFromDSRA {
  def apply(
             dsrap: DSRAProvider,
             order: Int,
             trainStreamSource: StreamSource,
             pMin: Double,
             alpha: Double,
             gamma: Double,
             r: Double
           ): SPSTmSourceFromDSRA = {
    val pstSource = PSTSourceLearnerFromDSRA(dsrap, trainStreamSource, order, pMin, alpha, gamma, r)
    SPSTmSourceFromDSRA(dsrap, order, pstSource)
  }

  def apply(
             dsrap: DSRAProvider,
             order: Int,
             pstSource: PSTSource
           ): SPSTmSourceFromDSRA = {
    val dsrais = dsrap.provide()
    val dsrapDirectI = DSRAProvider(DSRASourceDirectI(dsrais))
    val size = dsrais.size
    val orders = List.fill(size) {
      order
    }
    new SPSTmSourceFromDSRA(dsrapDirectI, orders, pstSource)
  }

  def apply(
             dsrap: DSRAProvider,
             orders: List[Int],
             pstSource: PSTSource
           ): SPSTmSourceFromDSRA = new SPSTmSourceFromDSRA(dsrap, orders, pstSource)
}

class SPSTmSourceFromDSRA(
                           val dsrap: DSRAProvider,
                           val orders: List[Int],
                           val pstSource: PSTSource
                         ) extends SPSTmSource {

}
