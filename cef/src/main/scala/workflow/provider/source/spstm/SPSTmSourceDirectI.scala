package workflow.provider.source.spstm

import fsm.SPSTmInterface

object SPSTmSourceDirectI {
  def apply(spstmi: List[SPSTmInterface]): SPSTmSourceDirectI = new SPSTmSourceDirectI(spstmi)
}

class SPSTmSourceDirectI(val spstmi: List[SPSTmInterface]) extends SPSTmSource {

}
