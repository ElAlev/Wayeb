package model.forecaster

import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.runtime.RelativeForecast

trait ForecasterInterface {
  def getNewForecast(
                      state: Int,
                      timestamp: Long
                    ): RelativeForecast
  def getStates: Set[Int]
  def getId: Int
  def getMaxSpread: Int
  def getType: ForecasterType
}
