package workflow.provider.source.sdfa

import fsm.CountPolicy.CountPolicy
import ui.ConfigUtils

object SDFASourceFromSRE {
  def apply(
             sreFile: String,
             policy: CountPolicy,
             declarations: String,
             minTermMethod: String
           ): SDFASourceFromSRE = new SDFASourceFromSRE(sreFile, policy, declarations, minTermMethod)

  def apply(
             sreFile: String,
             policy: CountPolicy,
             declarations: String
           ): SDFASourceFromSRE = new SDFASourceFromSRE(sreFile, policy, declarations, ConfigUtils.defaultMinTermMethod)
}

class SDFASourceFromSRE(
                         val sreFile: String,
                         val policy: CountPolicy,
                         val declarations: String,
                         val minTermMethod: String
                       ) extends SDFASource {

}
