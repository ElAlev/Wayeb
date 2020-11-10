package workflow.provider.source.forecaster

import model.forecaster.ForecasterInterface

object ForecasterSourceDirect {
  def apply(forecasters: List[ForecasterInterface]): ForecasterSourceDirect = new ForecasterSourceDirect(forecasters)
}

class ForecasterSourceDirect(val forecasters: List[ForecasterInterface]) extends ForecasterSource {

}
