package ui

import java.nio.file.{Files, Paths}
import fsm.{CountPolicy, FSMModel}
import model.waitingTime.ForecastMethod.ForecastMethod
import CountPolicy.CountPolicy
import fsm.FSMModel.FSMModel
import model.ProbModel
import model.ProbModel.ProbModel
import model.waitingTime.ForecastMethod

case class WayebConfig(
                        patterns: String = "",
                        declarations: String = "",
                        outputFsm: String = "",
                        outputMc: String = "",
                        outputSpst: String = "",
                        statsFile: String = "",
                        order: Int = ConfigUtils.defaultOrder,
                        confidenceThreshold: Double = ConfigUtils.defaultConfidenceThreshold,
                        maxSpread: Int = ConfigUtils.defaultMaxSpread,
                        horizon: Int = ConfigUtils.defaultHorizon,
                        foreMethod: ForecastMethod = ConfigUtils.defaultForeMethod,
                        maxNoStates: Int = ConfigUtils.maxNoStates,
                        pMin: Double = ConfigUtils.defaultPMin,
                        alpha: Double = ConfigUtils.defaultAlpha,
                        gammaMin: Double = ConfigUtils.defaultGammaMin,
                        r: Double = ConfigUtils.defaultR,
                        verbose: Boolean = ConfigUtils.defaultVerbose,
                        debug: Boolean = ConfigUtils.defaultDebug,
                        kafkaConf: String = ConfigUtils.defaultKafkaConf,
                        streamFile: String = "",
                        domainSpecificStream: String = "",
                        streamArgs: String = "",
                        policy: CountPolicy = ConfigUtils.defaultPolicy,
                        expirationTime: Int = ConfigUtils.defaultExpiration,
                        probModel: ProbModel = ConfigUtils.defaultProbModel,
                        fsmModel: FSMModel = ConfigUtils.defaultFsmModel,
                        fsmFile: String = "",
                        mcFile: String = "",
                        experimentsDomain: String = "cards",
                        task: String = "none",
                        reset: Boolean = false,
                        recOpt: Boolean = false,
                        warmupFirst: Boolean = ConfigUtils.warmupFirst,
                        warmupStreamSize: Int = ConfigUtils.warmupStreamSize,
                        findWarmupLimit: Boolean = ConfigUtils.findWarmupLimit,
                        batchLength: Int = ConfigUtils.batchLength,
                        measurements: Int = ConfigUtils.measurements,
                        show: Boolean = ConfigUtils.defaultShowMatchesForecasts,
                        postProcess: Boolean = ConfigUtils.defaultPostProcessMatches,
                        timeout: Long = ConfigUtils.defaultTimeout,
                        memoryTest: Boolean = ConfigUtils.defaultMemoryTest
)

object WayebCLI {
  def main(args: Array[String]): Unit = {

    val countPolicies = ConfigUtils.countPolicies
    val probModels = ConfigUtils.probModels
    val foreMethods = ConfigUtils.foreMethods
    val fsmModels = ConfigUtils.fsmModels
    val fsmModelsForecasting = ConfigUtils.fsmModelsForecasting

    val parser = new scopt.OptionParser[WayebConfig]("wayeb") {
      head("Wayeb", "0.6")

      help("help").text("prints this usage text")

      cmd("compile").
        action((_, c) => c.copy(task = "compile")).
        text("Compile and disambiguate FSM up to an order.").
        children(
          opt[String]("patterns").required().valueName("<file path>").
            action((x, c) => c.copy(patterns = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Pattern file does not exist")).
            text("The SRE(M) file for the patterns (required)."),
          opt[String]("outputFsm").required().valueName("<file path>").
            action((x, c) => c.copy(outputFsm = x)).
            text("Output file for the compiled/disambiguated FSM."),
          opt[String]("declarations").valueName("<file path>").
            action((x, c) => c.copy(declarations = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Declarations file does not exist")).
            text("File declarations (if any)."),
          opt[String]("countPolicy").valueName(countPolicies.toString()).
            action((x, c) => c.copy(policy = CountPolicy.str2Pol(x))).
            validate(x =>
              if (countPolicies.contains(x)) success
              else failure("Count policy should be one of " + countPolicies)).
            text("Counting policy."),
          opt[String]("fsmModel").valueName("One of " + fsmModels).
            action((x, c) => c.copy(fsmModel = FSMModel.string2FSMModel(x))).
            validate(x =>
              if (fsmModels.contains(x)) success
              else failure("FSM model should be one of " + fsmModels)).
            text("Specify the automaton model.")
        )

      cmd("mle").
        action((_, c) => c.copy(task = "mle")).
        text("Estimate transition matrix of PMC.").
        children(
          opt[String]("fsm").required().valueName("<file path>").
            action((x, c) => c.copy(fsmFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("FSM file does not exist")).
            text("FSM (serialized) to be used."),
          opt[String]("stream").required().valueName("<file path>").
            action((x, c) => c.copy(streamFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Stream file does not exist")).
            text("The input file with the stream of events (required)."),
          opt[String]("domainSpecificStream").valueName("<file path>").
            action((x, c) => c.copy(domainSpecificStream = x)).
            text("Specify the domain stream."),
          opt[String]("streamArgs").valueName("<file path>").
            action((x, c) => c.copy(streamArgs = x)).
            text("Specify the domain stream arguments (comma separated)."),
          opt[String]("outputMc").required().valueName("<file path>").
            action((x, c) => c.copy(outputMc = x)).
            text("Output file for the calculated Markov Chain."),
          opt[String]("kafkaConf").valueName("<file path>").
            action((x, c) => c.copy(kafkaConf = x)).
            text("Specify the configuration file for Kafka")
        )

      cmd("forecasting").
        action((_, c) => c.copy(task = "forecasting")).
        text("Process stream given a FSM and a learnt PMC (recognition and forecasting).").
        children(
          opt[String]("fsm").required().valueName("<file path>").
            action((x, c) => c.copy(fsmFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("FSM file does not exist")).
            text("FSM (serialized) to be used."),
          opt[String]("fsmModel").valueName("One of " + fsmModelsForecasting).
            action((x, c) => c.copy(fsmModel = FSMModel.string2FSMModel(x))).
            validate(x =>
              if (fsmModelsForecasting.contains(x)) success
              else failure("FSM model should be one of " + fsmModelsForecasting)).
            text("Specify the automaton model. Relevant only for VMMs."),
          opt[String]("mc").valueName("<file path>").
            action((x, c) => c.copy(mcFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("MC file does not exist")).
            text("Markov chain (serialized) to be used (relevant only for FMMs)."),
          opt[String]("modelType").required().valueName(probModels.toString()).
            action((x, c) => c.copy(probModel = ProbModel.string2ProbModel(x))).
            validate(x =>
              if (probModels.contains(x)) success
              else failure("Model type should be one of " + probModels)).
            text("Specify the type of the Markov model."),
          opt[String]("stream").required().valueName("<file path>").
            action((x, c) => c.copy(streamFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Stream file does not exist")).
            text("The input file with the stream of events (required)."),
          opt[String]("domainSpecificStream").valueName("<file path>").
            action((x, c) => c.copy(domainSpecificStream = x)).
            text("Specify the domain stream."),
          opt[String]("streamArgs").valueName("<file path>").
            action((x, c) => c.copy(streamArgs = x)).
            text("Specify the domain stream arguments (comma separated)."),
          opt[String]("statsFile").required().valueName("<file path>").
            action((x, c) => c.copy(statsFile = x)).
            text("Output file for statistics."),
          opt[Double]("threshold").valueName("Double >0 <=1.0").
            action((x, c) => c.copy(confidenceThreshold = x)).
            text("Forecasts produced should have probability above this threshold."),
          opt[Int]("maxSpread").valueName("Int >0").
            action((x, c) => c.copy(maxSpread = x)).
            text("Maximum spread allowed for a forecast."),
          opt[Int]("horizon").valueName("Int >0").
            action((x, c) => c.copy(horizon = x)).
            text("Horizon is the \"length\" of the waiting-time distributions, i.e., " +
              "for how may points into the future we want to calculate the completion probability."),
          opt[String]("foreMethod").valueName(foreMethods.toString()).
            action((x, c) => c.copy(foreMethod = ForecastMethod.string2method(x))).
            validate(x =>
              if (foreMethods.contains(x)) success
              else failure("Forecasting method should be one of " + foreMethods)).
            text("Forecasting method."),
          opt[String]("kafkaConf").valueName("<file path>").
            action((x, c) => c.copy(kafkaConf = x)).
            text("Specify the configuration file for Kafka")
        )

      cmd("recognition").
        action((_, c) => c.copy(task = "recognition")).
        text("Process stream given a FSM (recognition only).").
        children(
          opt[String]("fsm").required().valueName("<file path>").
            action((x, c) => c.copy(fsmFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("FSM file does not exist: " + x)).
            text("FSM (serialized) to be used."),
          opt[String]("stream").required().valueName("<file path>").
            action((x, c) => c.copy(streamFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x)) | x.equalsIgnoreCase("kafka")) success
              else failure("Stream file does not exist")).
            text("The input file with the stream of events (required)."),
          opt[String]("domainSpecificStream").valueName("<file path>").
            action((x, c) => c.copy(domainSpecificStream = x)).
            text("Specify the domain stream."),
          opt[String]("streamArgs").valueName("<file path>").
            action((x, c) => c.copy(streamArgs = x)).
            text("Specify the domain stream arguments (comma separated)."),
          opt[String]("statsFile").required().valueName("<file path>").
            action((x, c) => c.copy(statsFile = x)).
            text("Output file for statistics."),
          opt[String]("kafkaConf").valueName("<file path>").
            action((x, c) => c.copy(kafkaConf = x)).
            text("Specify the configuration file for Kafka"),
          opt[String]("fsmModel").valueName("One of " + fsmModels).
            action((x, c) => c.copy(fsmModel = FSMModel.string2FSMModel(x))).
            validate(x =>
              if (fsmModels.contains(x)) success
              else failure("FSM model should be one of " + fsmModels)).
            text("Specify the automaton model."),
          opt[Boolean]("reset").valueName("Boolean").
            action((x, c) => c.copy(reset = x)).
            text("Whether the engine should reset after every match."),
          opt[Boolean]("opt").valueName("Boolean").
            action((x, c) => c.copy(recOpt = x)).
            text("Whether optimizations should be enabled."),
          opt[Boolean]("warmupFirst").valueName("Boolean").
            action((x, c) => c.copy(warmupFirst = x)).
            text("Whether a warmup period should be used."),
          opt[Int]("warmupStreamSize").valueName("Int >0").
            action((x, c) => c.copy(warmupStreamSize = x)).
            text("Warmup period in number of input events."),
          opt[Boolean]("findWarmupLimit").valueName("Boolean").
            action((x, c) => c.copy(findWarmupLimit = x)).
            text("Whether this is to establish warmup period."),
          opt[Int]("batchLength").valueName("Int >0").
            action((x, c) => c.copy(batchLength = x)).
            text("Length of batches for finding warmup limit."),
          opt[Int]("measurements").valueName("Int >0").
            action((x, c) => c.copy(measurements = x)).
            text("Number of measurements for estimating throughput slope."),
          opt[Boolean]("show").valueName("Boolean").
            action((x, c) => c.copy(show = x)).
            text("Show complete matches?"),
          opt[Boolean]("postProcess").valueName("Boolean").
            action((x, c) => c.copy(postProcess = x)).
            text("Post-processing of complete matches?"),
          opt[Long]("timeout").valueName("Int > 0").
            action((x, c) => c.copy(timeout = x)).
            text("Timeout in seconds."),
          opt[Boolean]("mem").valueName("Boolean").
            action((x, c) => c.copy(memoryTest = x)).
            text("Memory test?"),
        )

      cmd("learnSPST").
        action((_, c) => c.copy(task = "learnSPST")).
        text("Learn symbolic probabilistic suffix automaton.").
        children(
          opt[String]("patterns").required().valueName("<file path>").
            action((x, c) => c.copy(patterns = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Pattern file does not exist")).
            text("The SRE file for the patterns (required)."),
          opt[String]("fsmModel").valueName("One of " + fsmModelsForecasting).
            action((x, c) => c.copy(fsmModel = FSMModel.string2FSMModel(x))).
            validate(x =>
              if (fsmModelsForecasting.contains(x)) success
              else failure("FSM model should be one of " + fsmModelsForecasting)).
            text("Specify the automaton model."),
          opt[Int]("pMin").valueName("pMin>0 and pMin<1.0").
            action((x, c) => c.copy(pMin = x)).
            text("This is the symbol threshold. Symbols with lower probability are discarded."),
          opt[Int]("alpha").valueName("alpha>0 and alpha<1.0").
            action((x, c) => c.copy(alpha = x)).
            text("Used to calculate the conditional threshold = (1 + alpha) * gammaMin.\n" +
              " The conditional on the expanded context must be greater than this threshold."),
          opt[Int]("gammaMin").valueName("gammaMin>0 and gammaMin<1.0").
            action((x, c) => c.copy(gammaMin = x)).
            text("Used to calculate the conditional threshold = (1 + alpha) * gammaMin.\n" +
              " The conditional on the expanded context must be greater than this threshold.\n" +
              "Also used for smoothing."),
          opt[Int]("r").valueName("r>0").
            action((x, c) => c.copy(r = x)).
            text("This is the likelihood ratio threshold.\n" +
              "Contexts are expanded if the probability ratio of the conditional on the expanded context by the\n " +
              " conditional on the original context is greater than this threshold."),
          opt[String]("declarations").valueName("<file path>").
            action((x, c) => c.copy(declarations = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Declarations file does not exist")).
            text("File declarations (if any)."),
          opt[String]("stream").required().valueName("<file path>").
            action((x, c) => c.copy(streamFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Stream file does not exist")).
            text("The input file with the training stream of events (required)."),
          opt[String]("domainSpecificStream").valueName("<file path>").
            action((x, c) => c.copy(domainSpecificStream = x)).
            text("Specify the domain stream."),
          opt[String]("streamArgs").valueName("<file path>").
            action((x, c) => c.copy(streamArgs = x)).
            text("Specify the domain stream arguments (comma separated)."),
          opt[String]("outputSpst").required().valueName("<file path>").
            action((x, c) => c.copy(outputSpst = x)).
            text("Output file for the SPST."),
          opt[String]("kafkaConf").valueName("<file path>").
            action((x, c) => c.copy(kafkaConf = x)).
            text("Specify the configuration file for Kafka")
        )

    }

    parser.parse(args, WayebConfig()) match {
      case Some(config) => runWayeb(config)
      case None => throw new IllegalArgumentException("Something is wrong with the arguments.")
    }

  }

  private def runWayeb(config: WayebConfig): Unit = {
    println(ConfigUtils.wayebLogo)
    println(ConfigUtils.wayebAscii)
    config.task match {
      case "compile" => BeepBeep.runCompile2FSM(config)
      case "mle" => BeepBeep.runMatrixEstimation(config)
      case "forecasting" => BeepBeep.runForecasting(config)
      case "recognition" => BeepBeep.runRecognition(config)
      case "learnSPST" => BeepBeep.runLearnSPST(config)
      case _ => throw new IllegalArgumentException("Unrecognized task.")
    }
  }

}
