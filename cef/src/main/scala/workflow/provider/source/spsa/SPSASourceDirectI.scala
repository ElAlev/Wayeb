package workflow.provider.source.spsa

import fsm.SPSAInterface

object SPSASourceDirectI {
  def apply(spsai: List[SPSAInterface]): SPSASourceDirectI = new SPSASourceDirectI(spsai)
}

class SPSASourceDirectI(val spsai: List[SPSAInterface]) extends SPSASource {

}
