package workflow.provider.source.sdfa

import fsm.CountPolicy.CountPolicy
import fsm.classical.pattern.regexp.RegExpTree
import fsm.symbolic.logic.{Predicate, Sentence}
import ui.ConfigUtils

object SDFASourceRegExp {
  def apply(
             re: RegExpTree,
             order: Int,
             partitionAttribute: String,
             window: Int,
             exclusives: Set[Set[Predicate]],
             extras: Set[Sentence],
             policy: CountPolicy
           ): SDFASourceRegExp = new SDFASourceRegExp(re, order, partitionAttribute, window, exclusives, extras, policy, ConfigUtils.defaultMinTermMethod)

  def apply(
             re: RegExpTree,
             order: Int,
             partitionAttribute: String,
             window: Int,
             exclusives: Set[Set[Predicate]],
             extras: Set[Sentence]
           ): SDFASourceRegExp = new SDFASourceRegExp(re, order, partitionAttribute, window, exclusives, extras, ConfigUtils.defaultPolicy, ConfigUtils.defaultMinTermMethod)
}

class SDFASourceRegExp(
                        val re: RegExpTree,
                        val order: Int,
                        val partitionAttribute: String,
                        val window: Int,
                        val exclusives: Set[Set[Predicate]],
                        val extras: Set[Sentence],
                        val policy: CountPolicy,
                        val minTermMethod: String
                      ) extends SDFASource {

}
