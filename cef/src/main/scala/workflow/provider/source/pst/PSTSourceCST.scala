package workflow.provider.source.pst

import model.vmm.mapper.Isomorphism
import model.vmm.pst.CounterSuffixTree

object PSTSourceCST {
  def apply(
             cstIsosOrder: List[(CounterSuffixTree, Isomorphism, Int)],
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): PSTSourceCST = new PSTSourceCST(cstIsosOrder, pMin, alpha, gammaMin, r)

  def apply(
             cstIsos: List[(CounterSuffixTree, Isomorphism)],
             maxorder: Int,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): PSTSourceCST = {
    val cstIsosOrder = cstIsos.map( ci => (ci._1, ci._2, maxorder))
    new PSTSourceCST(cstIsosOrder, pMin, alpha, gammaMin, r)
  }
}

class PSTSourceCST(
                    val cstIsosOrder: List[(CounterSuffixTree, Isomorphism, Int)],
                    val pMin: Double,
                    val alpha: Double,
                    val gammaMin: Double,
                    val r: Double
                  ) extends PSTSource {

}
