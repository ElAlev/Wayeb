package workflow.provider.source.spst

import fsm.SPSTInterface

object SPSTSourceDirectI {
  def apply(spsti: List[SPSTInterface]): SPSTSourceDirectI = new SPSTSourceDirectI(spsti)
}

class SPSTSourceDirectI(val spsti: List[SPSTInterface]) extends SPSTSource {

}
