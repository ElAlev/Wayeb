package model.forecaster

import estimator.HMMEstimator.IsoHMM
import fsm.{FSMInterface, SDFAInterface}
import model.vmm.Symbol
import model.forecaster.ForecasterType.ForecasterType
import model.forecaster.runtime.RelativeForecast
import model.vmm.pst.CyclicBuffer
import model.waitingTime.ForecastMethod.ForecastMethod
import model.waitingTime.{ForecastMethod, WtDistribution}

object HMMInterface {
  /**
    * Constructor for HMM interface.
    *
    * @param id The id of the FSM for which this predictor was constructed.
    * @param method REGRESSION or CLASSIFICATION
    * @param threshold The confidence threshold.
    * @param maxSpread The maximum spread.
    * @param horizon The horizon.
    * @param isohmm The HMM along with its corresponding isomorphism.
    * @param fsm The FSM.
    * @return The HMM interface.
    */
  def apply(
             id: Int,
             method: ForecastMethod,
             threshold: Double,
             maxSpread: Int,
             horizon: Int,
             isohmm: IsoHMM,
             fsm: FSMInterface
           ): HMMInterface =
    fsm match {
      case _: SDFAInterface => new HMMInterface(id, method, threshold, maxSpread, horizon, isohmm, fsm.asInstanceOf[SDFAInterface])
      case _ => throw new IllegalArgumentException("HMMInterface accepts only SDFAInterface")
    }

}

/**
  * Interface for HMM predictors.
  *
  * We use a HMM to describe the behavior of an automaton, constructed from a given pattern. The observation variable of
  * the HMM corresponds to the states of the pattern automaton, i.e., an observation sequence of length l for the HMM
  * consists of the sequence of states visited by the automaton after consuming l SDEs. We can train a HMM for an
  * automaton with the Baum-Welch algorithm, using the automaton to generate a training observation sequence from the
  * original training stream. We can then use this learned HMM to produce forecasts on a test dataset. We produce
  * forecasts in an online manner as follows: as the stream is consumed, we use a buffer to store the last l states
  * visited by the pattern automaton. After every new event, we "unroll" the HMM using the contents of the buffer as the
  * observation sequence and the transition and emission matrices learned during the training phase. We can then
  * estimate the probability of all possible future observation sequences (up to some length), which, in our case,
  * correspond to future states visited by the automaton. Knowing the probability of every future sequence of states
  * allows us to estimate the waiting-time distribution for the current state of the automaton and thus build a forecast
  * interval, as already described. Note that, contrary to the other predictors, the estimation of the waiting-time
  * distribution via a HMM must be performed online. We cannot pre-compute the waiting-time distributions and store the
  * forecasts in a look-up table, due to the possibly large number of entries. For example, assume that l = 5 and the
  * size of the "alphabet" of our automaton is 10. For each state of the automaton, we would have to pre-compute 10^^5
  * entries. In other words, as with Markov chains, we still have a problem of combinatorial explosion. The upside with
  * using HMMs is that we can at least estimate the waiting-time distribution, even if this is possible only in an
  * online manner.
  *
  * @param id The id of the FSM for which this predictor was constructed.
  * @param method REGRESSION or CLASSIFICATION
  * @param threshold The confidence threshold.
  * @param maxSpread The maximum spread.
  * @param horizon The horizon.
  * @param isohmm The HMM along with its corresponding isomorphism.
  * @param fsm The FSM.
  */
class HMMInterface(
                    val id: Int,
                    method: ForecastMethod,
                    threshold: Double,
                    maxSpread: Int,
                    horizon: Int,
                    isohmm: IsoHMM,
                    fsm: SDFAInterface
                  ) extends ForecasterInterface {

  // We need to decide how deep we want to unroll the HMM. Since we have a stream, we cannot unroll it all the way back
  // to the start of the stream. As an alternative, from each state we find the path (shortest, since there might
  // multiple paths) that leads to a final state and then keep the longest one.
  private val longestShortestPath = fsm.findShortestPathDistances.values.max
  // The buffer of observed states.
  private val buffer: CyclicBuffer = new CyclicBuffer(longestShortestPath + 1)

  private val encodedFinals: Set[Int] = fsm.getFinals.map(s => isohmm.stateEncoding(s))
  private val encodedNonFinals: Set[Int] = fsm.getNonFinals.map(s => isohmm.stateEncoding(s))

  /**
    * Estimates a forecast interval from the observed visited states for a given current state.
    *
    * @param state The id of the given state.
    * @param eventCounter The event counter (index) of the event that triggered the forecast.
    * @return The forecast.
    */
  override def getNewForecast(
                                 state: Int,
                                 eventCounter: Long
                               ): RelativeForecast = {
    // first retrieve the past observed states
    val observations = buffer.pop.map(s => s.value)
    // build waiting-time distributions (must be done online for every new forecast)
    val wt = buildWt(observations)
    // estimate the forecast interval
    val pred = wt.buildForecast(0, method, threshold, maxSpread)
    RelativeForecast(startRelativeToNow     = pred.start,
                       endRelativeToNow       = pred.end,
                       middle                 = pred.middle,
                       prob                   = pred.prob,
                       startRelativeToCounter = eventCounter + pred.start,
                       endRelativeToCounter   = eventCounter + pred.end,
                       isPositive             = pred.positive
    )
  }

  /**
    * Adds a new observed state to the buffer.
    *
    * @param state The id of the new FSM state.
    */
  def addObservation(state: Int): Unit = {
    // first encode the state so that it has a proper id, suitable for the HMM
    val encodedState = isohmm.stateEncoding(state)
    buffer.pushSymbol(Symbol(encodedState))
  }

  /**
    * Estimates the waiting-time distribution.
    *
    * @param observations The buffered observations.
    * @return The waiting-time distribution.
    */
  private def buildWt(observations: List[Int]): WtDistribution = {
    val hmm = isohmm.hmm
    val obsProb: Double = hmm.p(observations.reverse.toArray)
    var wt: Map[Int, Double] = Map.empty
    var expandedObservations: Set[List[Int]] = Set(observations)
    for (i <- 1 to horizon) {
      // first expand/project observations one step ahead
      val expansions = expand(expandedObservations)
      // gather the expansions that lead us to a final state
      val finalsExpansions = expansions._1
      // estimate the probabilities of these expansions
      val probs: List[Double] = finalsExpansions.toList.map(o => hmm.p(o.reverse.toArray))
      val probsWithoutNan: List[Double] = probs.filter(p => !p.isNaN)
      // sum these probabilities to finals
      val probSum = probsWithoutNan.sum
      // find the conditional probability of reaching a final given the initial observations.
      val condProb = probSum / obsProb
      val assignedProb = if (condProb.isNaN) 0.0 else condProb
      wt += (i -> assignedProb)
      expandedObservations = expansions._2
    }
    WtDistribution(wt)
  }

  /**
    * Projects a set of observations one step ahead.
    *
    * @param observationsSet The set of observations to be expanded.
    * @return The set of projected observations, given as two sets: one that leads to final states and one that leads to
    *         non-final states.
    */
  private def expand(observationsSet: Set[List[Int]]): (Set[List[Int]], Set[List[Int]]) = {
    expandAux(observationsSet.toList, (Set.empty, Set.empty))
  }

  /**
    * Helper recursive function to find projections.
    *
    * @param observations The list of observations (each observation a list of ints) to be expanded.
    * @param acc Helper accumulator
    * @return The set of projected observations, given as two sets: one that leads to final states and one that leads to
    *         non-final states.
    */
  @scala.annotation.tailrec
  private def expandAux(
                         observations: List[List[Int]],
                         acc: (Set[List[Int]], Set[List[Int]])
                       ): (Set[List[Int]], Set[List[Int]]) = {
    observations match {
      case Nil => acc
      case head :: tail => {
        val (newFinalsExpansions, newNonFinalsExpansion): (Set[List[Int]], Set[List[Int]]) = expand(head)
        expandAux(tail, (acc._1 ++ newFinalsExpansions, acc._2 ++ newNonFinalsExpansion))
      }
    }
  }

  /**
    * Projects a single observation sequence one step ahead.
    *
    * @param observations The observation sequence.
    * @return The set of projected observation sequences, given as two sets: one that leads to final states and one that
    *         leads to non-final states.
    */
  private def expand(observations: List[Int]): (Set[List[Int]], Set[List[Int]]) = {
    val finalsExpansions = encodedFinals.map(f => f :: observations)
    val nonFinalsExpansions = encodedNonFinals.map(f => f :: observations)
    (finalsExpansions, nonFinalsExpansions)
  }

  override def getId: Int = id

  override def getMaxSpread: Int = maxSpread

  override def getStates: Set[Int] = fsm.getStates

  override def getType: ForecasterType = {
    if (ForecastMethod.isClassification(method)) ForecasterType.CLASSIFICATION
    else ForecasterType.REGRESSION
  }
}
