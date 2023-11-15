package model.forecaster.runtime

trait ForecasterPrototype {
  def cloneForecaster(runId: Int): ForecasterRun
  def getInterfaceId: Int
}
