package utils

import com.typesafe.scalalogging.LazyLogging

/**
  * Helps to show progress of a process.
  */
object Progressor {
  /**
    * Constructor for progressor.
    *
    * @param name The name of the process/algorithm whose progress is to be monitored.
    * @param work2do The total work to do in units/steps.
    * @param significant The gap between consecutive progresses to be shown.
    * @return A new progressor.
    */
  def apply(
             name: String,
             work2do: Int,
             significant: Int
           ): Progressor = new Progressor(name, work2do, significant)

  /**
    * Constructor for progressor. Assumes steps of 1% in progress reports.
    *
    * @param name The name of the process/algorithm whose progress is to be monitored.
    * @param work2do The total work to do in units/steps.
    * @return A new progressor.
    */
  def apply(
             name: String,
             work2do: Int
           ): Progressor = new Progressor(name, work2do, 1)
}

/**
  * Shows percentage of work completed for a given process/algorithm. A process is assumed to require k units of work/
  * steps. Method utils.Progressor#tick() must be called after every unit of work has been completed. The progress is
  * updated if the new progress differs from the old more than the significant percentage (e.g., if significant=5, then
  * the progress will go from 0 to 5 to 10 to 15% etc, without showing intermediate progress values).
  *
  * @param name The name of the process/algorithm whose progress is to be monitored.
  * @param work2do The total work to do in units/steps.
  * @param significant The gap between consecutive progresses to be shown.
  */
class Progressor(
                  name: String,
                  work2do: Int,
                  significant: Int
                ) extends LazyLogging {
  require(significant >= 0 & significant < 50)
  private var lastProgress = 0
  private var newProgress = 0
  private var counter = 0

  /**
    * Do NOT forget to call this method after every one of the work2do steps has been completed.
    *
    * @return The new progress.
    */
  def tick: Int = {
    counter += 1
    newProgress = ((counter.toDouble / work2do.toDouble) * 100).toInt
    if ((counter == 1) | (significant == 0) | ((newProgress - lastProgress) >= significant)) {
      logger.info("[" + name + "] -> Progress: %3d%%".format(newProgress))
      //Thread.sleep(200)
      lastProgress = newProgress
    }
    newProgress
  }

}
