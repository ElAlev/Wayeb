package ui

import com.typesafe.config.{Config, ConfigFactory}
import fsm.CountPolicy.{CountPolicy, NONOVERLAP, OVERLAP}
import fsm.FSMModel
import fsm.FSMModel.FSMModel
import model.ProbModel
import model.ProbModel.ProbModel
import model.waitingTime.ForecastMethod
import model.waitingTime.ForecastMethod.ForecastMethod

object ConfigUtils {
  val countPolicies: Set[String] = Set("overlap", "nonoverlap")
  val probModels: Set[String] = Set("fmm", "vmm")
  val foreMethods: Set[String] = Set("argmax", "smart-scan", "classify-nextk")
  val fsmModels: Set[String] = Set("nsfa", "dsfa", "nsra", "dsra")
  val fsmModelsForecasting: Set[String] = Set("dsfa", "dsra")

  val config: Config = ConfigFactory.load()

  def defaultOrder: Int = config.getInt("default.order")
  def singlePartitionVal: String = config.getString("default.singlePartitionVal")
  def defaultPolicy: CountPolicy = config.getString("default.policy") match {
    case "overlap" => OVERLAP
    case _ => NONOVERLAP
  }
  def defaultHorizon: Int = config.getInt("default.horizon")
  def defaultMaxSpread: Int = config.getInt("default.maxSpread")
  def defaultMinDistance: Double = config.getDouble("default.minDistance")
  def defaultMaxDistance: Double = config.getDouble("default.maxDistance")
  def defaultDistance: (Double, Double) = (defaultMinDistance, defaultMaxDistance)
  def defaultForeMethod: ForecastMethod = ForecastMethod.string2method(config.getString("default.foreMethod"))
  def defaultExpiration: Int = config.getInt("default.expiration")
  def defaultPredictorEnabled: Boolean = config.getBoolean("default.predictorEnabled")
  def defaultFinalsEnabled: Boolean = config.getBoolean("default.finalsEnabled")
  def defaultNumberOfFolds: Int = config.getInt("default.numberOfFolds")
  def defaultConfidenceThreshold: Double = config.getDouble("default.confidenceThreshold")
  def maxCrossValOrder: Int = config.getInt("default.maxCrossValOrder")
  def defaultVerbose: Boolean = config.getBoolean("default.verbose")
  def defaultDebug: Boolean = config.getBoolean("default.debug")
  def defaultKafkaConf: String = config.getString("default.kafkaConf")
  def defaultCollectStats: Boolean = config.getBoolean("default.collectStats")
  def defaultProbModel: ProbModel = ProbModel.string2ProbModel(config.getString("default.probModel"))
  def defaultFsmModel: FSMModel = FSMModel.string2FSMModel(config.getString("default.fsmModel"))
  def defaultMinTermMethod: String = config.getString("default.minTermMethod")
  //def defaultCheckForEmitting: Boolean = config.getBoolean("default.checkForEmitting")
  def idGeneratorMax: Int = config.getInt("default.idGeneratorMax")
  def randPredSeed: Int = config.getInt("default.randPredSeed")
  def defaultShowMatchesForecasts: Boolean = config.getBoolean("default.showMatchesForecasts")
  def defaultPostProcessMatches: Boolean = config.getBoolean("default.postProcessMatches")

  def defaultTimeout: Long = config.getLong("default.timeout")
  def defaultMemoryTest: Boolean = config.getBoolean("default.memoryTest")
  def epsilonSymbol: String = config.getString("constants.epsilonSymbol")

  def symbolDistTolerance: Double = config.getDouble("vmm.tolerance")
  def epsilon: Double = config.getDouble("vmm.epsilon")
  def maxNoStates: Int = config.getInt("vmm.maxNoStates")
  def defaultPMin: Double = config.getDouble("vmm.pMin")
  def defaultAlpha: Double = config.getDouble("vmm.alpha")
  def defaultGammaMin: Double = config.getDouble("vmm.gammaMin")
  def defaultR: Double = config.getDouble("vmm.r")
  def wtCutoffThreshold: Double = config.getDouble("vmm.wtCutoffThreshold")

  def warmupFirst: Boolean = config.getBoolean("warmup.warmupFirst")
  def warmupStreamSize: Int = config.getInt("warmup.warmupStreamSize")
  def findWarmupLimit: Boolean = config.getBoolean("warmup.findWarmupLimit")
  def batchLength: Int = config.getInt("warmup.batchLength")
  def measurements: Int = config.getInt("warmup.measurements")

  def defaultDB: String = config.getString("default.db")
  def write2db: Boolean = config.getBoolean("default.write2db")
  def detectionsSchema: String = config.getString("wayebdb.detectionsSchemaName")
  def detectionsTable: String = config.getString("wayebdb.detectionsTableName")
  def forecastsSchema: String = config.getString("wayebdb.forecastsSchemaName")
  def forecastsTable: String = config.getString("wayebdb.forecastsTableName")

  def noOfPatterns: Int = config.getInt("testing.noOfPatterns")
  def noOfWPatterns: Int = config.getInt("testing.noOfWPatterns")
  def patternMaxDepth: Int = config.getInt("testing.patternMaxDepth")
  def wordMaxLength: Int = config.getInt("testing.wordMaxLength")
  def maxUnrollingWindow: Int = config.getInt("testing.maxUnrollingWindow")
  def maxOrder: Int = config.getInt("testing.maxOrder")
  def symbolsNo: Int = config.getInt("testing.symbolsNo")
  def symbolStreamSize: Int = config.getInt("testing.symbolStreamSize")
  def gammaMin: Double = config.getDouble("testing.gammaMin")

  def consistencyTolerance: Double = config.getDouble("markov.consistencyTolerance")

  // How much we allow the middle of a forecast interval to deviate from its start and end points.
  // The middle is estimated as the conditional expectation of the wt distribution given the interval.
  // This means that, especially for point forecasts, the middle might end up being slightly smaller than the start.
  def intervalTolerance: Double = config.getDouble("predictions.intervalTolerance")

  // How much two corresponding points in two distributions may differ so that they can be considered equal.
  def wtDistTolerance: Double = config.getDouble("wt.distributionTolerance")

  def wayebLogo: String = config.getString("misc.logo")
  def wayebAscii: String = config.getString("misc.ascii")

}
