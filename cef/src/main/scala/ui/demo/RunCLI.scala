package ui.demo

import ui.WayebCLI

object RunCLI {

  final val home: String = System.getenv("WAYEB_HOME")
  final val dataDir: String = home + "/data/maritime/"
  final val resultsDir: String = home + "/results"
  final val testDatasetFilename: String = dataDir + "227592820.csv"
  final val trainDatasetFilename: String = dataDir + "227592820.csv"
  final val patternFile: String = home + "/patterns/maritime/port/pattern.sre"
  final val declarationsFile: String = home + "/patterns/maritime/port/declarationsDistance1.sre"
  final val domain: String = "maritime"
  final val policy: String = "nonoverlap"
  final val finalsEnabled: String = "false"
  final val threshold = 0.5
  final val maxSpread = 5
  final val horizon = 50
  final val spreadMethod = "classify-nextk"

  def main(args: Array[String]): Unit = {
    runDis("testfmm")
    runRec("testfmm")
    runTrainFMM("testfmm")
    runTestFMM("testfmm")
  }

  private def runDis(patternName: String): Unit = {
    val fsm = resultsDir + "/" + patternName + ".fsm"
    val argsTest: Array[String] = Array(
      "compile",
      "--patterns:" + patternFile,
      "--declarations:" + declarationsFile,
      "--countPolicy:" + policy,
      "--outputFsm:" + fsm
    )
    WayebCLI.main(argsTest)
  }

  private def runRec(patternName: String): Unit = {
    val fsm = resultsDir + "/" + patternName + ".fsm"
    val stats = resultsDir + "/" + patternName + ".stats.rec"
    val argsTest: Array[String] = Array(
      "recognition",
      "--fsm:" + fsm,
      "--stream:" + testDatasetFilename,
      "--domainSpecificStream:" + domain,
      "--streamArgs:",
      "--statsFile:" + stats
    )
    WayebCLI.main(argsTest)
  }

  private def runTrainFMM(patternName: String): Unit = {
    val fsm = resultsDir + "/" + patternName + ".fsm"
    val mc = resultsDir + "/" + patternName + ".mc"
    val argsTrain: Array[String] = Array(
      "mle",
      "--fsm:" + fsm,
      "--stream:" + trainDatasetFilename,
      "--domainSpecificStream:" + domain,
      "--streamArgs:",
      "--outputMc:" + mc
    )
    WayebCLI.main(argsTrain)
  }

  private def runTestFMM(patternName: String): Unit = {
    val fsm = resultsDir + "/" + patternName + ".fsm"
    val stats = resultsDir + "/" + patternName + ".stats"
    val mc = resultsDir + "/" + patternName + ".mc"
    val argsTest: Array[String] = Array(
      "forecasting",
      "--threshold:" + threshold,
      "--maxSpread:" + maxSpread,
      "--spreadMethod:" + spreadMethod,
      "--horizon:" + horizon,
      "--modelType:fmm",
      "--fsm:" + fsm,
      "--mc:" + mc,
      "--stream:" + testDatasetFilename,
      "--domainSpecificStream:" + domain,
      "--streamArgs:",
      "--statsFile:" + stats
    )
    WayebCLI.main(argsTest)
  }


}
