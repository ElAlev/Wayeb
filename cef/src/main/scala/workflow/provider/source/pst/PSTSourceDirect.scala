package workflow.provider.source.pst

import model.vmm.mapper.{Isomorphism, SymbolMapper}
import model.vmm.pst.PredictionSuffixTree

object PSTSourceDirect {

  def apply(
             pst: List[(PredictionSuffixTree, SymbolMapper)]
           ): PSTSourceDirect = new PSTSourceDirect( pst)
}

class PSTSourceDirect(val pst: List[(PredictionSuffixTree, SymbolMapper)]) extends PSTSource {

}
