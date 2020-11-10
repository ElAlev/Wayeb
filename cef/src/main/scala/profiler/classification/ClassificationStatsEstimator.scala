package profiler.classification

import com.typesafe.scalalogging.LazyLogging
import profiler.StatsEstimator
import com.github.tototoshi.csv.CSVWriter

/**
  * Estimates various classification statistics from a list of forecast collectors.
  * CAUTION: Metrics that cannot be estimated (e.g., because the denominator in their formula is zero) have a value of
  * -1.0.
  *
  * @param collectors The collectors.
  */
class ClassificationStatsEstimator(collectors: List[ClassificationForecastCollector])
  extends StatsEstimator with LazyLogging {
  //private var perPartitionValueMetrics: List[(Double, Double, Double, Double, Double, Double, Double, Double)] = List.empty
  private var totalMetrics: (Double, Double, Double, Double, Double, Double, Double, Double) =
    (-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0)
  private var totalCounts: (Int, Int, Int, Int) = (-1, -1, -1, -1)

  /**
    * Estimates precision, recall, f1, specificity, accuracy, npv, informedness, Matthews correlation coefficient from
    * the collectors.
    */
  override def estimateStats(): Unit = {
    val counts = collectors.map(c => c.getTotalCounts)
    //perPartitionValueMetrics = counts.map(c => estimateMetrics(c._1, c._2, c._3, c._4))
    totalCounts = counts.fold((0, 0, 0, 0)) {
      (acc, x) => (acc._1 + x._1, acc._2 + x._2, acc._3 + x._3, acc._4 + x._4)
    }
    totalMetrics = estimateMetrics(totalCounts._1, totalCounts._2, totalCounts._3, totalCounts._4)
  }

  /**
    * Estimates per state statistics. Not currently implemented.
    */
  override def estimatePerStateStats(): Unit = {}

  /**
    * Estimates precision, recall, f1, specificity, accuracy, npv, informedness, Matthews correlation coefficient from
    * counts of true positives, true negatives, false positives and false negatives.
    *
    * @param tp The number of true positives.
    * @param tn The number of true negatives.
    * @param fp The number of false positives.
    * @param fn The number of false negatives.
    * @return (precision, recall, f1, specificity, accuracy, npv, informedness, Matthews correlation coefficient)
    */
  private def estimateMetrics(
                               tp: Int,
                               tn: Int,
                               fp: Int,
                               fn: Int
                             ): (Double, Double, Double, Double, Double, Double, Double, Double) = {
    val tpfp = tp + fp
    val precision = if (tpfp != 0) tp.toDouble / tpfp else -1
    val tpfn = tp + fn
    val recall = if (tpfn != 0) tp.toDouble / tpfn else -1
    val f1 = if (precision != -1 & recall != -1) (2 * precision * recall) / (precision + recall) else -1
    val tnfp = tn + fp
    val specificity = if (tnfp != 0) tn.toDouble / tnfp else -1
    val total = tp + tn + fp + fn
    val accuracy = if (total != 0) (tp + tn).toDouble / total else -1
    val tnfn = tn + fn
    val npv = if (tnfn != 0) tn.toDouble / tnfn else -1
    val informedness = if (recall != -1 & specificity != -1) recall + specificity - 1 else -1
    // do not use this formula to calculate mcc, denominator can easily overflow if the number of predictions is large
    //val mcc = if (tpfp != 0 & tpfn != 0 & tnfp != 0 & tnfn != 0) ((tp * tn).toDouble - (fp * fn) )/( math.sqrt(tpfp * tpfn * tnfp * tnfn)) else -1
    val mcc =
      if (tpfp == 0 | tpfn == 0 | tnfp == 0 | tnfn == 0) 0.0
      else {
        val fdr = 1 - precision
        val fnr = 1 - recall
        val fpr = 1 - specificity
        val fomr = 1 - npv
        math.sqrt(precision * recall * specificity * npv) - math.sqrt(fdr * fnr * fpr * fomr)
      }
    (precision, recall, f1, specificity, accuracy, npv, informedness, mcc)
  }

  /**
    * Prints all statistics.
    */
  override def printProfileInfo(): Unit = {
    val precision = totalMetrics._1
    val recall = totalMetrics._2
    val f1 = totalMetrics._3
    val specificity = totalMetrics._4
    val accuracy = totalMetrics._5
    val npv = totalMetrics._6
    val informedness = totalMetrics._7
    val mcc = totalMetrics._8
    logger.info("\n***** FORECASTING STATS *****\n")
    logger.info("\tPrecision: " + precision)
    logger.info("\tNegative predictive value (precision of other class): " + npv)
    logger.info("\tRecall: " + recall)
    logger.info("\tSpecificity (recall of other class): " + specificity)
    logger.info("\tF1: " + f1)
    logger.info("\tAccuracy: " + accuracy)
    logger.info("\tInformedness: " + informedness)
    logger.info("\tMatthews correlation coefficient: " + mcc)
    logger.info("\tCounts (tp/tn/fp/fn): " + totalCounts)
  }

  /**
    * Prints statistics and writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  override def printProfileInfo(fn: String): Unit = {
    printProfileInfo()
    writeProfileInfo(fn)
  }

  /**
    * Prints statistics per state. Not currently implemented.
    */
  override def printPerStateProfileInfo(): Unit = logger.warn("Per state stats not yet available")

  /**
    * Prints statistics per state and writes them to a csv file. Not currently implemented.
    */
  override def printPerStateProfileInfo(fn: String): Unit = logger.warn("Per state stats not yet available")

  /**
    * Retrieves the value for a certain metric.
    * Make sure stats have been estimated before trying to retrieve one. You must have called
    * profiler.classification.ClassificationPatternStats.estimateStats.
    *
    * @param which The metric:
    *              - precision;
    *              - recall;
    *              - f1;
    *              - specificity;
    *              - accuracy;
    *              - npv;
    *              - informedness;
    *              - mcc;
    *              - tp;
    *              - tn;
    *              - fp;
    *              - fn.
    *
    * @return The metric's value as a string.
    */
  override def getStat(which: String): String = {
    which match {
      case "precision" => totalMetrics._1.toString
      case "recall" => totalMetrics._2.toString
      case "f1" => totalMetrics._3.toString
      case "specificity" => totalMetrics._4.toString
      case "accuracy" => totalMetrics._5.toString
      case "npv" => totalMetrics._6.toString
      case "informedness" => totalMetrics._7.toString
      case "mcc" => totalMetrics._8.toString
      case "tp" => totalCounts._1.toString
      case "tn" => totalCounts._2.toString
      case "fp" => totalCounts._3.toString
      case "fn" => totalCounts._4.toString
      case _ => throw new IllegalArgumentException("Stat not recognized: " + which)
    }
  }

  /**
    * Writes statistics to a csv file.
    *
    * @param filename The path to the file.
    */
  private def writeProfileInfo(filename: String): Unit = {
    val writer = CSVWriter.open(filename, append = true)
    val precision = totalMetrics._1
    val recall = totalMetrics._2
    val f1 = totalMetrics._3
    val specificity = totalMetrics._4
    val accuracy = totalMetrics._5
    val npv = totalMetrics._6
    val informedness = totalMetrics._7
    val mcc = totalMetrics._8
    val tp = totalCounts._1
    val tn = totalCounts._2
    val fp = totalCounts._3
    val fn = totalCounts._4
    val row = List(
      precision.toString,
      recall.toString,
      f1.toString,
      specificity.toString,
      accuracy.toString,
      npv.toString,
      informedness.toString,
      mcc.toString,
      tp.toString,
      tn.toString,
      fp.toString,
      fn.toString
    )
    writer.writeRow(row)
    writer.close()
  }

}
