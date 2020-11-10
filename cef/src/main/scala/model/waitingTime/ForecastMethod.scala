package model.waitingTime

object ForecastMethod extends Enumeration {
  type ForecastMethod = Value
  val ARGMAX, FULLSCAN, SMARTSCAN, FIXEDSPREAD, CLASSIFY_NEXTK, CLASSIFY_WIN = Value

  def string2method(str: String): ForecastMethod = {
    str match {
      case "argmax" => ARGMAX
      case "full-scan" => FULLSCAN
      case "smart-scan" => SMARTSCAN
      case "fixed-spread" => FIXEDSPREAD
      case "classify-nextk" => CLASSIFY_NEXTK
      case "classify-win" => CLASSIFY_WIN
      case _ => throw new IllegalArgumentException("Forecast method not recognized " + str)
    }
  }

  def isClassification(fm: ForecastMethod): Boolean = fm == CLASSIFY_NEXTK | fm == CLASSIFY_WIN

  def isRegression(fm: ForecastMethod): Boolean = !isClassification(fm)
}
