package estimator.RemainingTimeEstimator

import breeze.stats.distributions.Gaussian
import fsm.FSMInterface
import model.waitingTime.WtDistribution

/**
  * Class that represents remaining times models.
  *
  * @param initRts The initial remaining times.
  */
class RemainingTimes private[RemainingTimeEstimator] (initRts: Map[Int, (Double, Double)]) {
  // for each state, we maintain the mean remaining time and the standard deviation
  private var rts: Map[Int, (Double, Double)] = initRts

  /**
    * Updates the list of remaining times for a given state.
    *
    * @param state The state which will be updated.
    * @param mu The mean remaining time.
    * @param stddev The standard deviation.
    */
  def updateState(
                   state: Int,
                   mu: Double,
                   stddev: Double
                 ): Unit = {
    //require(rts.contains(state))
    val muStd = (mu, stddev)
    rts += (state -> muStd)
  }

  /**
    * Mean getter for a given state
    *
    * @param state The state
    * @return The mean
    */
  def getMuForState(state: Int): Double = {
    require(rts.contains(state))
    rts(state)._1
  }

  def getStddevForState(state: Int): Double = {
    require(rts.contains(state))
    rts(state)._2
  }

  def getStates: Set[Int] = rts.keySet

  /**
    * Calculates the waiting-time distributions based on the mean remaining times. The wt distribution is a "fitted"
    * gaussian, based on the mean remaining time and the standard deviation.
    *
    * @param fsm The FSM for which we will calculate the wt distributions. We need to pass it so that we know the final
    *            states.
    * @param horizon The horizon of the wt distribution.
    * @param finalsEnabled If true, we will estimate the wt distributions for the final states as well.
    * @return The wt distributions, one for each state.
    */
  def computeWTDists(
                      fsm: FSMInterface,
                      horizon: Int,
                      finalsEnabled: Boolean
                    ): Map[Int, WtDistribution] = {
    val nonFinals = fsm.getNonFinals
    val retained: Map[Int, (Double, Double)] = if (finalsEnabled) rts else rts.filter(x => nonFinals.contains(x._1))
    val gaussians = retained.mapValues(v => new Gaussian(v._1, v._2))
    val wtds = gaussians.mapValues(g => gaussian2wt(g, horizon))
    //val outwtds = wtds.filter(x => x._2.sum > 0.95)
    //outwtds
    wtds
  }

  /**
    * Converts a gaussian to a wt distribution. A wt distribution is a discrete one. We thus find the probabilities of
    * all the discrete points until the horizon.
    *
    * @param gd The gaussian distribution.
    * @param horizon The horizon.
    * @return The wt distribution.
    */
  private def gaussian2wt(
                           gd: Gaussian,
                           horizon: Int
                         ): WtDistribution = {
    val timepoints = (1 to horizon).toList
    val probs = timepoints.map(t => gd.probability(t - 0.5, t + 0.5))
    val wt = timepoints.zip(probs).toMap
    WtDistribution(wt)
  }

  override def toString: String = rts.toString()

}
