package workflow.provider.source.spst

import stream.source.StreamSource
import workflow.provider.SDFAProvider
import workflow.provider.source.pst.{PSTSource, PSTSourceLearnerFromSDFA}
import workflow.provider.source.sdfa.SDFASourceDirectI

object SPSTSourceFromSDFA {
  def apply(
             sdfap: SDFAProvider,
             order: Int,
             trainStreamSource: StreamSource,
             pMin: Double,
             alpha: Double,
             gamma: Double,
             r: Double
           ): SPSTSourceFromSDFA = {
    val pstSource = PSTSourceLearnerFromSDFA(sdfap, trainStreamSource, order, pMin, alpha, gamma, r)
    SPSTSourceFromSDFA(sdfap, order, pstSource)
  }

  def apply(
             sdfap: SDFAProvider,
             order: Int,
             pstSource: PSTSource
           ): SPSTSourceFromSDFA = {
    val sdfais = sdfap.provide()
    val sdfapDirectI = SDFAProvider(SDFASourceDirectI(sdfais))
    val size  = sdfais.size
    val orders = List.fill(size){order}
    new SPSTSourceFromSDFA(sdfapDirectI, orders, pstSource)
  }

  def apply(
             sdfap: SDFAProvider,
             orders: List[Int],
             pstSource: PSTSource
           ): SPSTSourceFromSDFA = new SPSTSourceFromSDFA(sdfap, orders, pstSource)
}

class SPSTSourceFromSDFA(
                          val sdfap: SDFAProvider,
                          val orders: List[Int],
                          val pstSource: PSTSource,
                        ) extends SPSTSource {
}
