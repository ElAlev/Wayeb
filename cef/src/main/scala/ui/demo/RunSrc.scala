package ui.demo

import model.waitingTime.ForecastMethod
import stream.StreamFactory
import ui.ConfigUtils
import workflow.provider.source.forecaster.ForecasterSourceBuild
import workflow.provider.source.matrix.MCSourceMLE
import workflow.provider.source.sdfa.SDFASourceFromSRE
import workflow.provider.source.wt.WtSourceMatrix
import workflow.provider._
import workflow.task.engineTask.ERFTask

object RunSrc extends App {
  final val confidenceThreshold = 0.4
  final val horizon = 50
  final val domain = "maritime"
  final val maxSpread = 10
  final val method = ForecastMethod.CLASSIFY_NEXTK
  final val distance = (0.0001, 0.5)

  final val home = System.getenv("WAYEB_HOME")
  final val dataDir: String = home + "/data/maritime/"
  final val resultsDir: String = home + "/results"
  final val testDatasetFilename: String = dataDir + "227592820.csv"
  final val trainDatasetFilename: String = dataDir + "227592820.csv"
  final val patternFile: String = home + "/patterns/maritime/port/pattern.sre"
  final val declarationsFile: String = home + "/patterns/maritime/port/declarationsDistance1.sre"

  // First create the training and test stream sources.
  // For convenience, here we use the same file, but should be different in real experiments.
  val streamTrainSource = StreamFactory.getDomainStreamSource(trainDatasetFilename, domain = domain, List.empty)
  val streamTestSource = StreamFactory.getDomainStreamSource(testDatasetFilename, domain = domain, List.empty)

  // Create a provider for the SDFA
  val sdfap = SDFAProvider(SDFASourceFromSRE(patternFile, ConfigUtils.defaultPolicy, declarationsFile))
  // Wrap a FSM provider around it
  val fsmp = FSMProvider(sdfap)
  // Create a provider for the Markov chain model
  val mp = MarkovChainProvider(MCSourceMLE(fsmp, streamTrainSource))
  // Create a provider for the waiting-time distributions
  val wtp = WtProvider(WtSourceMatrix(fsmp, mp, horizon = horizon, finalsEnabled = false))
  // Create a provider for the forecast intervals
  val pp = ForecasterProvider(ForecasterSourceBuild(
    fsmp,
    wtp,
    horizon             = horizon,
    confidenceThreshold = confidenceThreshold,
    maxSpread           = maxSpread,
    method              = ForecastMethod.CLASSIFY_NEXTK
  ))

  // Now execute recognition and forecasting
  val erft = ERFTask(
    fsmp             = fsmp,
    pp               = pp,
    predictorEnabled = true,
    finalsEnabled    = false,
    expirationDeadline   = ConfigUtils.defaultExpiration,
    distance         = distance,
    streamSource     = streamTestSource,
    collectStats = true,
    show = true
  )
  val prof = erft.execute()
  prof.printProfileInfo()

  val f1score = prof.getStatFor("f1", 0)
  println("\n\n\n\n\n\tF1-score: " + f1score)

}
