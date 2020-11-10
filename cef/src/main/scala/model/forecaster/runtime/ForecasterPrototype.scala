package model.forecaster.runtime

trait ForecasterPrototype {
  def cloneForecaster: ForecasterRun
  def getId: Int
}
