package workflow.provider

import java.io.{FileInputStream, ObjectInputStream}
import model.waitingTime.ForecastMethod.ForecastMethod
import model.forecaster.{HMMInterface, NextInterface, ForecasterInterface, RandomInterface, WtInterface}
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source._
import forecaster.{ForecasterHMMSourceBuild, ForecasterNextSourceBuild, ForecasterSource, ForecasterSourceBuild, ForecasterSourceDirect, ForecasterSourceRandom, ForecasterSourceSerialized}
import workflow.task.predictorTask.{HMMPredictorTask, PredictorNextTask, PredictorRandomTask, WtPredictorTask}

object ForecasterProvider {
  /**
    * Constructor for Forecaster provider when forecasterSource is not
    * workflow.provider.source.forecaster.ForecasterSourceSerialized.
    *
    * @param forecasterSource The source type, other than workflow.provider.source.forecaster.ForecasterSourceSerialized.
    * @return A forecaster provider.
    */
  def apply(forecasterSource: ForecasterSource): ForecasterProvider =
    new ForecasterProvider(forecasterSource, List.empty[Condition])

  /**
    * Constructor for Forecaster provider when forecasterSource is
    * workflow.provider.source.forecaster.ForecasterSourceSerialized.
    *
    * @param forecasterSource The source type, must be workflow.provider.source.forecaster.ForecasterSourceSerialized.
    * @return A forecaster provider.
    */
  def apply(forecasterSource: ForecasterSourceSerialized): ForecasterProvider =
    new ForecasterProvider(forecasterSource, List(new FileExistsCondition(forecasterSource.fn)))
}

/**
  * According to type of source for predictor, builds a provider in the form of a list of predictor interfaces.
  *
  * @param forecasterSource The source type:
  *                        - ForecasterSourceDirect, when an already existing predictor is given.
  *                        - ForecasterSourceBuild, when forecasts need to be estimated.
  *                        - ForecasterSourceRandom for generating random forecast intervals.
  *                        - ForecasterNextSourceBuild for next top k forecasting.
  *                        - ForecasterHMMSourceBuild for HMM forecasters.
  *                        - ForecasterSourceSerialized if the forecasters are serialiazed and stored.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class ForecasterProvider private(
                                  forecasterSource: ForecasterSource,
                                  conditions: List[Condition]
                                ) extends AbstractProvider(conditions) {

  /**
    * Calling this function actually initiates the construction of the forecaster interfaces.
    *
    * @return A list of forecaster interfaces.
    */
  override def provide(): List[ForecasterInterface] = {
    forecasterSource match {
      case x: ForecasterSourceDirect => x.forecasters
      case x: ForecasterSourceBuild => estimateForecasts(x.fsmp, x.wtdp, x.horizon, x.confidenceThreshold, x.maxSpread, x.method)
      case x: ForecasterSourceRandom => estimateRandomForecasts(x.fsmp, x.horizon)
      case x: ForecasterNextSourceBuild => estimateNextForecasts(x.fsmp, x.mcp)
      case x: ForecasterHMMSourceBuild => setupHMMForecasters(x.fsmp, x.hmmp, x.horizon, x.confidenceThreshold, x.maxSpread, x.method)
      case x: ForecasterSourceSerialized => deserializeFile(x.fn)
      case _ => throw new Error("Not valid PredictorSource")
    }
  }

  /**
    * Estimates forecast intervals.
    *
    * @param fsmp The provider for the FSMs.
    * @param wtdp The provider for the waiting-time distributions.
    * @param horizon The horizon.
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The maximum spread.
    * @param method The forecasting method.
    * @return A list of forecaster interfaces.
    */
  private def estimateForecasts(
                                 fsmp: FSMProvider,
                                 wtdp: WtProvider,
                                 horizon: Int,
                                 confidenceThreshold: Double,
                                 maxSpread: Int,
                                 method: ForecastMethod
                               ): List[WtInterface] = {
    val pt = WtPredictorTask(fsmp, wtdp, horizon, confidenceThreshold, maxSpread, method)
    pt.execute()._1
  }

  /**
    * Creates HMM forecasters.
    *
    * @param fsmp The provider for the FSMs.
    * @param hmmp The provider for the HMMs.
    * @param horizon The horizon.
    * @param confidenceThreshold The confidence threshold.
    * @param maxSpread The maximum spread.
    * @param method The forecasting method.
    * @return A list of forecaster interfaces.
    */
  private def setupHMMForecasters(
                                   fsmp: FSMProvider,
                                   hmmp: HMMProvider,
                                   horizon: Int,
                                   confidenceThreshold: Double,
                                   maxSpread: Int,
                                   method: ForecastMethod
                                 ): List[HMMInterface] = {
    val pt = HMMPredictorTask(fsmp, hmmp, horizon, confidenceThreshold, maxSpread, method)
    pt.execute()._1
  }

  /**
    * Estimates random forecast intervals.
    *
    * @param fsmp The provider for the FSMs.
    * @param horizon The horizon.
    * @return A list of forecaster interfaces.
    */
  private def estimateRandomForecasts(
                                       fsmp: FSMProvider,
                                       horizon: Int
                                     ): List[RandomInterface] = {
    val prt = PredictorRandomTask(fsmp, horizon)
    prt.execute()
  }

  /**
    * Estimates next top k forecasts.
    *
    * @param fsmp The provider for the FSMs.
    * @param mcp The provider for the Markov chains.
    * @return A list of forecaster interfaces.
    */
  private def estimateNextForecasts(
                                     fsmp: FSMProvider,
                                     mcp: MarkovChainProvider
                                   ): List[NextInterface] = {
    val pnt = PredictorNextTask(fsmp, mcp)
    pnt.execute()
  }

  /**
    * Deserializes a list of waiting-time forecaster interfaces.
    *
    * @param fn The path to the file with the serialized forecasters.
    * @return A list of waiting-time forecaster interfaces.
    */
  private def deserializeFile(fn: String): List[WtInterface] = {
    val oisWTI = new ObjectInputStream(new FileInputStream(fn))
    val wti = oisWTI.readObject.asInstanceOf[List[WtInterface]]
    oisWTI.close()
    wti
  }
}
