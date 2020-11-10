package workflow.provider.source.sdfa

import fsm.SDFAInterface

object SDFASourceDirectI {
  def apply(sdfai: List[SDFAInterface]): SDFASourceDirectI = new SDFASourceDirectI(sdfai)

}

class SDFASourceDirectI(val sdfai: List[SDFAInterface]) extends SDFASource {

}
