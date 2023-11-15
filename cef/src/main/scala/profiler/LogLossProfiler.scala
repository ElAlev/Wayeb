package profiler

import com.typesafe.scalalogging.LazyLogging
import com.github.tototoshi.csv.CSVWriter

object LogLossProfiler {
  /**
    * Constructor for log-loss profiler.
    *
    * @param avgLogLoss The average log-loss.
    * @param sequenceLength The length of the test sequence.
    * @return The log-loss profiler.
    */
  def apply(
             avgLogLoss: Double,
             sequenceLength: Int
           ): LogLossProfiler = new LogLossProfiler(avgLogLoss, sequenceLength)
}

/**
  * This is a profiler for log-loss experiments. We are only interested in the average log-loss on a test sequence.
  *
  * @param avgLogLoss The average log-loss.
  * @param sequenceLength The length of the test sequence.
  */
class LogLossProfiler(
                       val avgLogLoss: Double,
                       val sequenceLength: Int
                     ) extends ProfilerInterface with LazyLogging {

  /**
    * Statistics already available, nothing to do.
    */
  override def estimateStats(): Unit = {}

  /**
    * Prints log-loss and sequence length.
    */
  override def printProfileInfo(): Unit = {
    logger.info("\n***** LOG-LOSS STATS *****")
    logger.info("\tAverage log-loss: " + "%.5f".format(avgLogLoss))
    logger.info("\tSequence length : " + sequenceLength)
  }

  /**
    * Prints statistics and writes them to a file.
    *
    * @param fn The path to the file.
    */
  override def printProfileInfo(fn: String): Unit = {
    printProfileInfo()
    writeProfileInfo("", fn)
  }

  /**
    * Prints statistics and writes them to a file. Also adds a prefix as first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  override def printProfileInfo(
                                 prefix: String,
                                 fn: String
                               ): Unit = {
    printProfileInfo()
    writeProfileInfo(prefix, fn)
  }

  /**
    * Writes statistics to a file and adds the prefix in first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  def writeProfileInfo(
                        prefix: String,
                        fn: String
                      ): Unit = {
    val writer = CSVWriter.open(fn, append = true)
    val row = List(prefix, avgLogLoss.toString, sequenceLength.toString)
    writer.writeRow(row)
    writer.close()
  }

}
