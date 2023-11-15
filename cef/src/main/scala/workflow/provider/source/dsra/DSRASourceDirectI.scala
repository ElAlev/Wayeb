package workflow.provider.source.dsra

import fsm.DSRAInterface

object DSRASourceDirectI {
  def apply(dsrai: List[DSRAInterface]): DSRASourceDirectI = new DSRASourceDirectI(dsrai)
}

class DSRASourceDirectI(val dsrai: List[DSRAInterface]) extends DSRASource {

}
