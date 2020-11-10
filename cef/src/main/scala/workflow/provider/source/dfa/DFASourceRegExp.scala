package workflow.provider.source.dfa

import fsm.CountPolicy.CountPolicy
import fsm.classical.pattern.regexp.RegExpTree
import ui.ConfigUtils

object DFASourceRegExp {
  def apply(
             re: RegExpTree,
             policy: CountPolicy,
             order: Int,
             streamSymbols: Set[String],
             partitionAttribute: String
           ): DFASourceRegExp = new DFASourceRegExp(
    re,
    policy,
    order,
    streamSymbols,
    partitionAttribute
  )

  def apply(
             re: RegExpTree,
             policy: CountPolicy,
             order: Int,
             streamSymbols: Set[String]
           ): DFASourceRegExp = new DFASourceRegExp(
    re,
    policy,
    order,
    streamSymbols,
    ConfigUtils.singlePartitionVal
  )
}

class DFASourceRegExp(
                       val re: RegExpTree,
                       val policy: CountPolicy,
                       val order: Int,
                       val streamSymbols: Set[String],
                       val partitionAttribute: String
                     ) extends DFASource {

}
