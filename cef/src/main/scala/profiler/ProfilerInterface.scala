package profiler

/**
  * Every profiler must implement this trait.
  */
trait ProfilerInterface {
  /**
    * Prints all calculated statistics.
    */
  def printProfileInfo(): Unit

  /**
    * Prints all calculated statistics and also writes them to a csv file.
    *
    * @param fn The path to the file.
    */
  def printProfileInfo(fn: String): Unit

  /**
    * Prints all calculated statistics and also writes them to a csv file. Additionally, the prefix will be written in
    * the first column.
    *
    * @param prefix The prefix.
    * @param fn The path to the file.
    */
  def printProfileInfo(prefix: String, fn: String): Unit

  /**
    * Should be called after stream has been consumed to estimate statistics and make them available for printing and
    * retrieval.
    */
  def estimateStats(): Unit
}
