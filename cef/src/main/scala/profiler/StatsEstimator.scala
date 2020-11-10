package profiler

abstract class StatsEstimator {
  def printProfileInfo(): Unit
  def printProfileInfo(fn: String): Unit
  def printPerStateProfileInfo(): Unit
  def printPerStateProfileInfo(fn: String): Unit
  def getStat(which: String): String
  def estimateStats(): Unit
  def estimatePerStateStats(): Unit
}
