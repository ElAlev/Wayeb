package ui

import fsm.FSMModel.FSMModel
import fsm.{DSRAInterface, FSMModel, NSRAInterface, SDFAInterface, SNFAInterface, SPSTInterface, SPSTmInterface}
import model.ProbModel
import model.ProbModel.ProbModel
import profiler.{ProfilerInterface, WtProfiler}
import stream.source.StreamSource
import stream.StreamFactory
import utils.SerializationUtils
import workflow.provider._
import workflow.provider.source.dsra.DSRASourceSerialized
import workflow.provider.source.matrix.MCSourceSerialized
import workflow.provider.source.forecaster.ForecasterSourceBuild
import workflow.provider.source.nsra.NSRASourceSerialized
import workflow.provider.source.sdfa.SDFASourceSerialized
import workflow.provider.source.snfa.SNFASourceSerialized
import workflow.provider.source.spst.{SPSTSourceDirectI, SPSTSourceSerialized}
import workflow.provider.source.spstm.SPSTmSourceDirectI
import workflow.provider.source.wt.{WtSourceMatrix, WtSourceSPST, WtSourceSPSTm}
import workflow.task.engineTask.{ERFOptTask, ERFTask}
import workflow.task.estimatorTask.MatrixMLETask
import workflow.task.fsmTask.{DSRATask, NSRATask, SDFATask, SNFATask, SPSTTask, SPSTmTask}

object BeepBeep {

  private[ui] def runCompile2FSM(config: WayebConfig): Unit = {
    config.probModel match {
      case ProbModel.FMM => {
        config.fsmModel match {
          case FSMModel.DSFA => runSDFADisambiguation(config)
          case FSMModel.DSRA => runDSRAcompilation(config)
          case FSMModel.NSFA => runSNFACompilation(config)
          case FSMModel.NSRA => runNSRAcompilation(config)
          case _ => throw new IllegalArgumentException
        }
      }
      case _ => throw new IllegalArgumentException
    }
  }

  private def runSDFADisambiguation(config: WayebConfig): Unit = {
    val sdfat = SDFATask(config.patterns, config.declarations, config.policy)
    val sdfa = sdfat.execute()
    SerializationUtils.write2File[SDFAInterface](sdfa, config.outputFsm)
  }

  private def runSNFACompilation(config: WayebConfig): Unit = {
    val snfat = SNFATask(config.patterns)
    val snfa = snfat.execute()
    SerializationUtils.write2File[SNFAInterface](snfa, config.outputFsm)
  }

  private def runDSRAcompilation(config: WayebConfig): Unit = {
    val dsrat = DSRATask(config.patterns, config.declarations)
    val dsra = dsrat.execute()
    SerializationUtils.write2File[DSRAInterface](dsra, config.outputFsm)
  }

  private def runNSRAcompilation(config: WayebConfig): Unit = {
    val nsrat = NSRATask(config.patterns)
    val nsra = nsrat.execute()
    SerializationUtils.write2File[NSRAInterface](nsra, config.outputFsm)
  }

  private[ui] def runMatrixEstimation(config: WayebConfig): Unit = {
    val trainStream = getStreamSource(config.streamFile, config.domainSpecificStream, config.kafkaConf, config.streamArgs)
    val fsmp = getFSMProviderSerialized(config.fsmModel, config.probModel, config.fsmFile)
    val met = MatrixMLETask(fsmp, trainStream)
    val mcs = met.execute()._1
    SerializationUtils.write2File(mcs, config.outputMc)
  }

  private[ui] def runForecasting(config: WayebConfig): Unit = {
    val fsmp = getFSMProviderSerialized(config.fsmModel, config.probModel, config.fsmFile)
    val pp = config.probModel match {
      case ProbModel.VMM => {
        config.fsmModel match {
          case FSMModel.DSFA => {
            val spsti = fsmp.provide().map(x => x.asInstanceOf[SPSTInterface])
            val spstp = SPSTProvider(SPSTSourceDirectI(spsti))
            val wtp = WtProvider(
              WtSourceSPST(
                spstp,
                horizon         = config.horizon,
                cutoffThreshold = ConfigUtils.wtCutoffThreshold,
                distance        = ConfigUtils.defaultDistance
              )
            )
            ForecasterProvider(
              ForecasterSourceBuild(
                fsmp,
                wtp,
                horizon             = config.horizon,
                confidenceThreshold = config.confidenceThreshold,
                maxSpread           = config.maxSpread,
                method              = config.foreMethod
              )
            )
          }
          case FSMModel.DSRA => {
            val spstmi = fsmp.provide().map(x => x.asInstanceOf[SPSTmInterface])
            val spstpm = SPSTmProvider(SPSTmSourceDirectI(spstmi))
            val wtp = WtProvider(
              WtSourceSPSTm(
                spstpm,
                horizon         = config.horizon,
                cutoffThreshold = ConfigUtils.wtCutoffThreshold,
                distance        = ConfigUtils.defaultDistance
              )
            )
            ForecasterProvider(
              ForecasterSourceBuild(
                fsmp,
                wtp,
                horizon             = config.horizon,
                confidenceThreshold = config.confidenceThreshold,
                maxSpread           = config.maxSpread,
                method              = config.foreMethod
              )
            )
          }
        }
      }
      case ProbModel.FMM => {
        val mcp = MarkovChainProvider(MCSourceSerialized(config.mcFile))
        val wtdp = WtProvider(WtSourceMatrix(fsmp, mcp, config.horizon, ConfigUtils.defaultFinalsEnabled))
        ForecasterProvider(ForecasterSourceBuild(fsmp, wtdp, config.horizon, config.confidenceThreshold, config.maxSpread, config.foreMethod))
      }
    }
    val stream = getStreamSource(config.streamFile, config.domainSpecificStream, config.kafkaConf, config.streamArgs)
    val erf = ERFTask(fsmp, pp, stream, show = ConfigUtils.defaultShowMatchesForecasts)
    val profiler = erf.execute().asInstanceOf[ProfilerInterface]
    profiler.printProfileInfo(config.statsFile)
  }

  private[ui] def runRecognition(config: WayebConfig): Unit = {
    val fsmp = getFSMProviderSerialized(config.fsmModel, config.probModel, config.fsmFile)
    val stream = getStreamSource(config.streamFile, config.domainSpecificStream, config.kafkaConf, config.streamArgs)
    val erf = if (config.recOpt) ERFOptTask(fsmp, config.show, config.postProcess, config.warmupFirst, config.warmupStreamSize, config.findWarmupLimit, config.batchLength, config.measurements, stream, config.reset, config.timeout, config.memoryTest)
              else ERFTask(fsmp, stream, config.reset)
    val profiler = erf.execute().asInstanceOf[WtProfiler]
    profiler.printProfileInfo(config.statsFile)
  }

  private[ui] def runLearnSPST(config: WayebConfig): Unit = {
    val streamSource = getStreamSource(config.streamFile, config.domainSpecificStream, config.kafkaConf, config.streamArgs)
    config.fsmModel match {
      case FSMModel.DSFA => {
        val spstt = SPSTTask(config.patterns, config.declarations, config.policy, streamSource, config.pMin, config.alpha, config.gammaMin, config.r)
        val spst = spstt.execute()
        SerializationUtils.write2File[SPSTInterface](spst, config.outputSpst)
      }
      case FSMModel.DSRA => {
        val spstmt = SPSTmTask(config.patterns, config.declarations, streamSource, config.pMin, config.alpha, config.gammaMin, config.r)
        val spstm = spstmt.execute()
        val pst = spstm.head.pst
        println(pst.toString)
        SerializationUtils.write2File[SPSTmInterface](spstm, config.outputSpst)
      }
      case _ => throw new IllegalArgumentException("Unrecognized automaton model")
    }
  }

  private def getFSMProviderSerialized(
                                        fsmModel: FSMModel,
                                        probModel: ProbModel,
                                        fsmFile: String
                                      ): FSMProvider = {
    val fap: AbstractProvider = fsmModel match {
      case FSMModel.DSFA => probModel match {
        case ProbModel.FMM => SDFAProvider(SDFASourceSerialized(fsmFile))
        case ProbModel.VMM => SPSTProvider(SPSTSourceSerialized(fsmFile))
        case _ => throw new IllegalArgumentException
      }
      case FSMModel.DSRA => DSRAProvider(DSRASourceSerialized(fsmFile))
      case FSMModel.NSFA => SNFAProvider(SNFASourceSerialized(fsmFile))
      case FSMModel.NSRA => NSRAProvider(NSRASourceSerialized(fsmFile))
      case _ => throw new IllegalArgumentException
    }


    val fsmp = FSMProvider(fap)
    fsmp
  }

  private def getStreamSource(
                               fn: String,
                               domain: String,
                               kafkaConf: String,
                               args: String
                             ): StreamSource = {
    if (domain == "") StreamFactory.getCSVStreamSource(fn)
    else {
      val splitArgs = args.split(",").map(_.trim).toList
      StreamFactory.getDomainStreamSource(fn, domain, kafkaConf, splitArgs)
    }
  }

}
