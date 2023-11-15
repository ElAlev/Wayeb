package Specs.engine

import model.waitingTime.ForecastMethod
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.StreamFactory
import ui.ConfigUtils
import workflow.provider.source.dsra.DSRASourceFromSREM
import workflow.provider.source.forecaster.ForecasterSourceBuild
import workflow.provider.source.sdfa.SDFASourceFromSRE
import workflow.provider.{DSRAProvider, FSMProvider, ForecasterProvider, SDFAProvider, SPSTProvider, SPSTmProvider, WtProvider}
import workflow.provider.source.spst.{SPSTSourceDirectI, SPSTSourceFromSDFA}
import workflow.provider.source.spstm.{SPSTmSourceDirectI, SPSTmSourceFromDSRA}
import workflow.provider.source.wt.{WtSourceDirect, WtSourceSPST, WtSourceSPSTm}
import workflow.task.engineTask.ERFTask

@RunWith(classOf[JUnitRunner])
class SPSTvsSPSTm extends FlatSpec {
  "Engine running with SPST " should " produce same results as engine running with SPSTm " in {
    var confidenceThreshold = 0.5
    val horizon = 10
    val domain = "csv"
    val maxSpread = 10
    val method = ForecastMethod.CLASSIFY_NEXTK
    val distance = (0.0001, 1.0)

    val home = System.getenv("WAYEB_HOME")
    val dataDir: String = home + "/data/demo/"
    val resultsDir: String = home + "/results"
    val testDatasetFilename: String = dataDir + "data.csv"
    val trainDatasetFilename: String = dataDir + "data.csv"
    val patternFile: String = home + "/patterns/demo/a_seq_b.sre"
    val declarationsFile: String = home + "/patterns/demo/declarations.sre"

    // First create the training and test stream sources.
    // For convenience, here we use the same file, but should be different in real experiments.
    val streamTrainSource = StreamFactory.getDomainStreamSource(trainDatasetFilename, domain = domain, List.empty)
    val streamTestSource = StreamFactory.getDomainStreamSource(testDatasetFilename, domain = domain, List.empty)

    val pMin = 0.001
    val alpha = 0.0
    val gamma = 0.001
    val r = 1.05

    val sdfap = SDFAProvider(SDFASourceFromSRE(patternFile, ConfigUtils.defaultPolicy, declarationsFile))
    val fsmp = FSMProvider(sdfap)
    val spstp1 = SPSTProvider(SPSTSourceFromSDFA(sdfap, 1, streamTrainSource, pMin = pMin, alpha = alpha, gamma = gamma, r = r))
    val spstp = SPSTProvider(SPSTSourceDirectI(List(spstp1.provide().head)))
    val fsmp1 = FSMProvider(spstp)
    val wtp0 = WtProvider(WtSourceSPST(
      spstp,
      horizon = horizon,
      cutoffThreshold = ConfigUtils.wtCutoffThreshold,
      distance = distance
    ))
    val wtp1 = WtProvider(WtSourceDirect(List(wtp0.provide().head)))
    val pp1 = ForecasterProvider(ForecasterSourceBuild(
      fsmp1,
      wtp1,
      horizon = horizon,
      confidenceThreshold = confidenceThreshold,
      maxSpread = maxSpread,
      method = method
    ))

    val erft1 = ERFTask(
      fsmp = fsmp1,
      pp = pp1,
      predictorEnabled = true,
      finalsEnabled = false,
      expirationDeadline = ConfigUtils.defaultExpiration,
      distance = distance,
      streamSource = streamTestSource
    )
    val prof1 = erft1.execute()
    prof1.printProfileInfo()
    val f1score1 = prof1.getStatFor("f1", 0)
    println("\n\n\n\n\n\tF1-score: " + f1score1)


    val patternFilem: String = patternFile //home + "/patterns/demo/a_seq_b.sre"
    val declarationsFilem: String = declarationsFile //""
    confidenceThreshold = 0.625
    val dsrap = DSRAProvider(DSRASourceFromSREM(patternFilem, declarationsFilem))
    val spstpm1 = SPSTmProvider(SPSTmSourceFromDSRA(dsrap, 1, streamTrainSource, pMin = pMin, alpha = alpha, gamma = gamma, r = r))
    val spstpm = SPSTmProvider(SPSTmSourceDirectI(List(spstpm1.provide().head)))
    val fsmpm1 = FSMProvider(spstpm)
    val wtpm0 = WtProvider(WtSourceSPSTm(
      spstpm,
      horizon = horizon,
      cutoffThreshold = ConfigUtils.wtCutoffThreshold,
      distance = distance
    ))
    val wtpm1 = WtProvider(WtSourceDirect(List(wtpm0.provide().head)))
    val ppm1 = ForecasterProvider(ForecasterSourceBuild(
      fsmpm1,
      wtpm1,
      horizon = horizon,
      confidenceThreshold = confidenceThreshold,
      maxSpread = maxSpread,
      method = method
    ))

    val erftm1 = ERFTask(
      fsmp = fsmpm1,
      pp = ppm1,
      predictorEnabled = true,
      finalsEnabled = false,
      expirationDeadline = ConfigUtils.defaultExpiration,
      distance = distance,
      streamSource = streamTestSource
    )
    val profm1 = erftm1.execute()
    profm1.printProfileInfo()

    val f1score1m = profm1.getStatFor("f1", 0)
    println("\n\n\n\n\n\tF1-score: " + f1score1m)

  }

}
