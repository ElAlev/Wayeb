package profiler

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.LazyLogging
import fsm.runtime.MatchDump
import profiler.classification.{ClassificationForecastCollector, ClassificationStatsEstimator}
import profiler.regression.{RegressionForecastCollector, RegressionStatsEstimator}

object WtProfiler {
  /**
    * Constructor for a wt profiler.
    *
    * @return A wt profiler.
    */
  def apply(): WtProfiler = new WtProfiler()
}

/**
  * This is the standard profiler to be used for CE regression and classification forecasting.
  * profiler.WtProfiler#estimateStats() must be called before trying to print statistics or retrieve them.
  *
  * Profiling works in the following way:
  *
  * Each model.predictor.runtime.PredictorRun creates a profiler.ForecastCollector (either classification or regression).
  * The main job of a forecast collector is to collect the produced forecasts and detections of its run. Collectors
  * can also be queried in order to retrieve some basic statistics, e.g., the number of true positives thus far.
  *
  * After running forecasting, the collectors are grouped by pattern. See, e.g., engine.ERFEngine#shutdown(). These
  * grouped collectors are passed to a profiler (profiler.WtProfiler#setCollectors(scala.collection.immutable.Map)).
  * The profiler creates a profiler.StatsEstimator for each pattern. Therefore, each estimator will have a number of
  * collectors. Each such estimator, based on the basic stats provided by its collectors, can estimate some more
  * advanced stats for its pattern, e.g., the F1 score. So, in the end, the profiler has a number of estimators, one for
  * each pattern. Each estimator has a number of collectors, one for each run.
  *
  * Note that currently forecasting statistics are provided only on a per pattern basis.
  *
  */
class WtProfiler() extends ProfilerInterface with LazyLogging {
  private var streamSize = 0
  private var firstTimestamp: Long = 0
  private var lastTimestamp: Long = 0
  private var execTime: Long = 0
  private var matchesNo = 0
  private var lockedRuns = 0
  private var unlockedRuns = 0
  private var matchDump: MatchDump = _
  var statsEstimators: Map[Int, StatsEstimator] = Map.empty

  private var memTest: Boolean = false
  private var maxMemTotal: Long = 0
  private var avgMemTotal: Long = 0
  private var maxMemUsed: Long = 0
  private var avgMemUsed: Long = 0
  private var countMemMeas: Int = 0

  /**
    * Estimates statistics for all patterns.
    */
  override def estimateStats(): Unit = {
    statsEstimators.foreach(s => s._2.estimateStats())
    statsEstimators.foreach(s => s._2.estimatePerStateStats())
  }

  /**
    * Sets some global (for all patterns) statistics.
    *
    * @param streamSize The size of the stream.
    * @param execTime The total execution time in ns.
    * @param matchesNo The total number of matches (regardless of pattern).
    * @param lockedRuns The total number of locked runs.
    * @param unlockedRuns The total number of unlocked runs.
    * @param matchDump All detected matches as a dump.
    */
  def setGlobal(
                 streamSize: Int,
                 firstTimestamp: Long,
                 lastTimestamp: Long,
                 execTime: Long,
                 matchesNo: Int,
                 lockedRuns: Int,
                 unlockedRuns: Int,
                 matchDump: MatchDump
               ): Unit = {
    this.streamSize = streamSize
    this.firstTimestamp = firstTimestamp
    this.lastTimestamp = lastTimestamp
    this.execTime = execTime
    this.matchesNo = matchesNo
    this.lockedRuns = lockedRuns
    this.unlockedRuns = unlockedRuns
    this.matchDump = matchDump
  }

  def setMemStats(
                   maxMemTotal: Long,
                   avgMemTotal: Long,
                   maxMemUsed: Long,
                   avgMemUsed: Long,
                   countMemMeas: Int
                 ): Unit = {
    this.maxMemTotal = maxMemTotal
    this.avgMemTotal = avgMemTotal
    this.maxMemUsed = maxMemUsed
    this.avgMemUsed = avgMemUsed
    this.countMemMeas = countMemMeas
    memTest = true
  }

  /**
    * Creates the estimators.
    *
    * @param collectors The collectors, grouped by pattern id.
    */
  def createEstimators(collectors: Map[Int, List[ForecastCollector]]): Unit = {
    this.statsEstimators = collectors.values.head.head match {
      case _: RegressionForecastCollector => collectors.map(c => (c._1, new RegressionStatsEstimator(c._2.map(x => x.asInstanceOf[RegressionForecastCollector]))))
      case _: ClassificationForecastCollector => collectors.map(c => (c._1, new ClassificationStatsEstimator(c._2.map(x => x.asInstanceOf[ClassificationForecastCollector]))))
    }
  }

  /**
    * Retrieves the value for a certain statistic.
    *
    * @param which The statistic:
    *              - streamSize;
    *              - execTime;
    *              - matchesNo;
    *              - lockedRuns;
    *              - unlockedRuns.
    * @return The value, as string.
    */
  def getStat(which: String): String = {
    which match {
      case "streamSize" => streamSize.toString
      case "firstTimestamp" => firstTimestamp.toString
      case "lastTimestamp" => lastTimestamp.toString
      case "execTime" => execTime.toString
      case "matchesNo" => matchesNo.toString
      case "lockedRuns" => lockedRuns.toString
      case "unlockedRuns" => unlockedRuns.toString
      case _ => throw new IllegalArgumentException("Stat not recognized: " + which)
    }
  }

  /**
    * Retrieves the value for a certain statistic and for one of the patterns.
    *
    * @param which The statistic. Depends on type of estimator.
    *              See profiler.regression.RegressionStatsEstimator#getStat(java.lang.String) and
    *              profiler.classification.ClassificationStatsEstimator#getStat(java.lang.String).
    * @param forPattern The id of the pattern.
    * @return The value, as string.
    */
  def getStatFor(
                  which: String,
                  forPattern: Int
                ): String = statsEstimators(forPattern).getStat(which)

  /**
    * Prints recognition and forecasting stats.
    */
  def printProfileInfo(): Unit = {
    printRecStats()
    printPatternStats()
  }

  /**
    * Prints recognition and forecasting stats. Also prints per automaton states stats, if available.
    */
  def printProfileInfoWithPerStateStats(): Unit = {
    printProfileInfo()
    printPerStateStats()
  }

  /**
    * Prints all calculated statistics and also writes them to csv files.
    *
    * @param fn The path to the file.
    */
  def printProfileInfo(fn: String): Unit = {
    printRecStats(fn)
    printPatternStats(fn + "_pattern")
    //printPerStateStats(fn + "_pattern_per_state")
  }

  /**
    * Prints all calculated statistics and also writes them to a csv file. Additionally, the prefix will be written in
    * the first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  def printProfileInfo(
                        prefix: String,
                        fn: String
                      ): Unit = {
    printProfileInfo()
    writeProfileInfo(prefix, fn)
    //printPerStateStats(fn + "_per_state")
  }

  /**
    * Prints recognition stats.
    */
  def printRecStats(): Unit = {
    if (!memTest) {
      val throughput = (streamSize.toDouble / execTime.toDouble) * 1000000000
      val streamDuration = (lastTimestamp - firstTimestamp)
      if (lastTimestamp < firstTimestamp) {
        logger.warn("lastTimestamp < firstTimestamp")
      }
      //val improvementOverRealTime: Double = ((execTime.toDouble / 1000000000) / streamDuration) * 100
      logger.info("\n***** RECOGNITION STATS *****")
      logger.info("\tStream size: " + streamSize + " events")
      logger.info("\tExecution time: " + (execTime / 1000000) + "ms")
      logger.info("\tThroughput : " + throughput.toInt + " events/sec")
      //logger.info("\tStream duration : " + streamDuration + " sec")
      //logger.info("\tImprovement over real time : " + improvementOverRealTime.toInt + " %")
      logger.info("\tMatches No: " + matchesNo)
      //logger.info("\tLocked/Unlocked Runs: " + lockedRuns + "/" + unlockedRuns)
      val execTimeSec = execTime.toDouble / 1000000000
      //println(execTimeSec + "," + streamSize + "," + execTimeSec + "," + matchesNo + "," + execTimeSec + "," + throughput + "," + throughput)
    }
    else {
      val avgMemTotalkB = if (countMemMeas!=0) (avgMemTotal / countMemMeas) else -1
      val avgMemUsedkB = if (countMemMeas!=0) (avgMemUsed / countMemMeas) else -1
      logger.info("\n***** MEMORY STATS *****")
      logger.info("\tmaxMemTotal: " + maxMemTotal / 1024 + " kB")
      logger.info("\tavgMemTotal: " + avgMemTotalkB / 1024 + " kB")
      logger.info("\tmaxMemUsed: " + maxMemUsed / 1024 + " kB")
      logger.info("\tavgMemUsed: " + avgMemUsedkB / 1024 + " kB")
      //println(maxMemTotal + "," + avgMemTotalkB + "," + maxMemUsed + "," + avgMemUsedkB + "," + countMemMeas)
    }
  }

  /**
    * Prints recognition stats and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  def printRecStats(fn: String): Unit = {
    printRecStats()
    writeRecStats(fn)
  }

  /**
    * Prints forecasting statistics for each pattern.
    */
  def printPatternStats(): Unit =
    statsEstimators.foreach(s => { logger.info("\n\n\t ***** Pattern id: " + s._1 + "*****\n"); s._2.printProfileInfo() })

  /**
    * Prints forecasting statistics for each pattern and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  def printPatternStats(fn: String): Unit =
    statsEstimators.foreach(s => { logger.info("\n\n\t ***** Pattern id: " + s._1 + "*****\n"); s._2.printProfileInfo(fn) })

  /**
    * Prints forecasting statistics for each pattern and for each automaton state.
    */
  def printPerStateStats(): Unit = statsEstimators.foreach(p => p._2.printPerStateProfileInfo())

  /**
    * Prints forecasting statistics for each pattern and for each automaton state and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  def printPerStateStats(fn: String): Unit = statsEstimators.foreach(p => p._2.printPerStateProfileInfo(fn))

  /**
    * Writes recognition statistics to a csv file.
    *
    * @param fn The path to the file.
    */
  def writeRecStats(fn: String): Unit = {
    val writer = CSVWriter.open(fn + ".csv", append = true)
    val row = List(
      streamSize.toString,
      execTime.toString,
      matchesNo.toString,
      lockedRuns.toString,
      unlockedRuns.toString
    )
    writer.writeRow(row)
    writer.close()
  }

  /**
    * Writes recognition statistics to a file with a prefix at the first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  def writeProfileInfo(
                        prefix: String,
                        fn: String
                      ): Unit = {
    val writer = CSVWriter.open(fn, append = true)
    val row = List(
      prefix,
      streamSize.toString,
      execTime.toString,
      matchesNo.toString,
      lockedRuns.toString,
      unlockedRuns.toString
    )
    writer.writeRow(row)
    writer.close()
  }

  /**
    * Prints all matches.
    */
  def printMatches(): Unit = logger.info("\n" + matchDump.toString)

  /**
    * @return All matches.
    */
  def getMatchDump: MatchDump = matchDump

}
