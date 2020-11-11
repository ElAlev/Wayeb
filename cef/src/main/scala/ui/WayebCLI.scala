package ui

import java.nio.file.{Files, Paths}
import fsm.CountPolicy
import model.waitingTime.ForecastMethod.ForecastMethod
import CountPolicy.CountPolicy
import model.waitingTime.ForecastMethod

case class WayebConfig(
                        patterns: String = "",
                        declarations: String = "",
                        outputFsm: String = "",
                        outputMc: String = "",
                        statsFile: String = "",
                        order: Int = ConfigUtils.defaultOrder,
                        confidenceThreshold: Double = ConfigUtils.defaultConfidenceThreshold,
                        maxSpread: Int = ConfigUtils.defaultMaxSpread,
                        horizon: Int = ConfigUtils.defaultHorizon,
                        spreadMethod: ForecastMethod = ConfigUtils.defaultSpreadMethod,
                        verbose: Boolean = ConfigUtils.defaultVerbose,
                        debug: Boolean = ConfigUtils.defaultDebug,
                        isKafka: Boolean = ConfigUtils.defaultIsKafka,
                        kafkaConf: String = ConfigUtils.defaultKafkaConf,
                        streamFile: String = "",
                        domainSpecificStream: String = "",
                        streamArgs: String = "",
                        policy: CountPolicy = ConfigUtils.defaultPolicy,
                        expirationTime: Int = ConfigUtils.defaultExpiration,
                        modelType: String = ConfigUtils.defaultModelType,
                        fsmFile: String = "",
                        mcFile: String = "",
                        task: String = "none"
)

object WayebCLI {
  def main(args: Array[String]): Unit = {

    val countPolicies = ConfigUtils.countPolicies
    val modelTypes = ConfigUtils.modelTypes
    val spreadMethods = ConfigUtils.spreadMethods

    val parser = new scopt.OptionParser[WayebConfig]("wayeb") {
      head("Wayeb", "0.2")

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
            text("The SRE file for the patterns (required)."),
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
            text("Counting policy.")
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
          opt[Boolean]("isKafka").valueName("Boolean").
            action((x, c) => c.copy(isKafka = x)).
            text("Whether the source originates from Kafka or not."),
          opt[String]("kafkaConf").valueName("<file path>").
            action((x, c) => c.copy(kafkaConf = x)).
            text("Specify the configuration file for Kafka")
        )

      cmd("forecasting").
        action((_, c) => c.copy(task = "forecasting")).
        text("Process stream given a FSM and a learnt PMC (recognition and forecasting).").
        children(
          opt[String]("modelType").required().valueName(modelTypes.toString()).
            action((x, c) => c.copy(modelType = x)).
            validate(x =>
              if (modelTypes.contains(x)) success
              else failure("FSM type should be one of " + modelTypes)).
            text("FSM type."),
          opt[String]("fsm").required().valueName("<file path>").
            action((x, c) => c.copy(fsmFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("FSM file does not exist")).
            text("FSM (serialized) to be used."),
          opt[String]("mc").valueName("<file path>").
            action((x, c) => c.copy(mcFile = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("MC file does not exist")).
            text("Markov chain (serialized) to be used (only for FMMs)."),
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
          opt[String]("spreadMethod").valueName(spreadMethods.toString()).
            action((x, c) => c.copy(spreadMethod = ForecastMethod.string2method(x))).
            validate(x =>
              if (spreadMethods.contains(x)) success
              else failure("Spread method should be one of " + spreadMethods)).
            text("Spread method."),
          opt[Boolean]("isKafka").valueName("Boolean").
            action((x, c) => c.copy(isKafka = x)).
            text("Whether the source originates from Kafka or not."),
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
          opt[Boolean]("isKafka").valueName("Boolean").
            action((x, c) => c.copy(isKafka = x)).
            text("Whether the source originates from Kafka or not."),
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
    config.task match {
      case "compile" => BeepBeep.runFSMDisambiguation(config)
      case "mle" => BeepBeep.runMatrixEstimation(config)
      case "forecasting" => BeepBeep.runForecasting(config)
      case "recognition" => BeepBeep.runRecognition(config)
      case _ => throw new IllegalArgumentException("Unrecognized task.")
    }
  }

}
