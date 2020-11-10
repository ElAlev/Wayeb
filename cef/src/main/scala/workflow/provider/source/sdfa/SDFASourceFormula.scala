package workflow.provider.source.sdfa

import fsm.symbolic.sfa.logic.{Predicate, Sentence}
import fsm.symbolic.sre.SREFormula
import fsm.CountPolicy.CountPolicy
import ui.ConfigUtils

object SDFASourceFormula {
  def apply(
             formulas: List[(SREFormula, Int, String)],
             policy: CountPolicy,
             exclusives: Set[Set[Predicate]],
             extras: Set[Sentence],
             minTermMethod: String
           ): SDFASourceFormula = new SDFASourceFormula(
    formulas,
    policy,
    exclusives,
    extras,
    minTermMethod
  )

  def apply(
             formulas: List[(SREFormula, Int, String)],
             policy: CountPolicy,
             exclusives: Set[Set[Predicate]],
             extras: Set[Sentence]
           ): SDFASourceFormula = new SDFASourceFormula(
    formulas,
    policy,
    exclusives,
    extras,
    ConfigUtils.defaultMinTermMethod
  )
}

class SDFASourceFormula(
                         val formulas: List[(SREFormula, Int, String)],
                         val policy: CountPolicy,
                         val exclusives: Set[Set[Predicate]],
                         val extras: Set[Sentence],
                         val minTermMethod: String
                       ) extends SDFASource {

}
