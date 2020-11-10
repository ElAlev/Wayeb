package model.markov

import java.io.{FileOutputStream, ObjectOutputStream}

import breeze.linalg.{DenseMatrix, DenseVector}
import fsm.FSMInterface
import model.waitingTime.WtDistribution
import ui.ConfigUtils

import scala.collection.mutable

//TODO: [refactor] The markov package should be decoupled from the rest. Should be able to work as a standalone package.

object MarkovChain {

  /**
    * Markov chain constructor.
    *
    * @param matrix The transition matrix.
    * @param state2Row A map from FSM states to matrix rows.
    * @param absorbingNo The number of Markov absorbing states, equal to the number of FSM final states.
    * @return The Markov chain.
    */
  def apply(
             matrix: DenseMatrix[Double],
             state2Row: scala.collection.immutable.Map[Int, Int],
             absorbingNo: Int
           ): MarkovChain = {
    new MarkovChain(matrix, state2Row, absorbingNo)
  }

  /**
    * Markov chain constructor.
    *
    * @param fsm The FSM.
    * @param probs A set of conditional probabilities from which to build the transition matrix.
    * @return The Markov chain.
    */
  def apply(
             fsm: FSMInterface,
             probs: TransitionProbs
           ): MarkovChain = {
    val state2Row = fsm.buildState2Row
    val m: DenseMatrix[Double] = fsm.buildTransitionMatrix(probs)
    val mc = new MarkovChain(m, state2Row, fsm.getFinalsNo)
    mc
  }

}

/**
  * Class representing the Markov chain corresponding to a FSM.
  *
  * @param matrix The transition matrix.
  * @param state2Row A map from FSM states to matrix rows.
  * @param absorbingNo The number of Markov absorbing states, equal to the number of FSM final states.
  */
class MarkovChain private (
                            matrix: DenseMatrix[Double],
                            state2Row: scala.collection.immutable.Map[Int, Int],
                            absorbingNo: Int
                          ) extends Serializable {
  private val consistencyTolerance = ConfigUtils.consistencyTolerance
  require(checkMatrix(), "Transition matrix not consistent")

  /**
    * Estimates the probability of moving from a given FSM state to one of a number of target FSM states.
    *
    * @param from The FSM state from which we start.
    * @param to The set of FSM target states.
    * @return The transition probability.
    */
  def getTransProbToStates(
                            from: Int,
                            to: Set[Int]
                          ): Double = {
    val fromState = state2Row(from)
    val toStates = to.map(ts => state2Row(ts))
    getTransProbTo(fromState, toStates)
  }

  /**
    * Estimates the probability of moving from a given Markov state to one of a number of target Markov states.
    *
    * @param from The Markov state from which we start.
    * @param to The set of Markov target states.
    * @return The transition probability.
    */
  private def getTransProbTo(
                              from: Int,
                              to: Set[Int]
                            ): Double = to.map(t => matrix(from, t)).sum

  /**
    * Estimates the probability of moving from a given FSM state to another given FSM state.
    *
    * @param fromState The FSM source state.
    * @param toState The FSM target state.
    * @return The transition probability.
    */
  def getTransProbFromTo(
                          fromState: Int,
                          toState: Int
                        ): Double = {
    require(state2Row.contains(fromState))
    require(state2Row.contains(toState))
    matrix(state2Row(fromState), state2Row(toState))
  }

  /**
    * Estimates the waiting-time distributions for the states of a FSM,
    * according to the following theorems.
    *
    * Let Π be the transition probability matrix of a homogeneous Markov chain Yt in the form of Equation (1) and ξinit
    * its initial state distribution. The probability for the time index n when the system first enters the set of
    * states F , starting from a state in N , can be obtained from
    * P (Yn ∈ F, Yn−1 ∈/ F, ..., Y1 ∈/ F | ξinit) = ξN T Nn−1(I − N )1
    * where ξN is the vector consisting of the elements of ξinit corresponding to the states of N .
    *
    * Let Π be the transition probability matrix of a homogeneous Markov chain Yt in the form of Equation (1) and ξinit
    * its initial state distribution. The probability for the time index n when the system first enters the set of
    * states F , starting from a state in F , can be obtained from
    *
    * P (Yn ∈ F, Yn−1 ∈/ F, · · · , Y1 ∈ F | ξinit ) =
    *
    * TξF F 1 if n = 2
    *
    * ξFTFN Nn−2(I − N )1 otherwise
    *
    * where ξF is the vector consisting of the elements of ξinit corresponding to the states of F .
    *
    * @param fsm The given FSM corresponding to the Markov chain.
    * @param horizon The horizon of the distributions.
    * @param finalsEnabled If true, the distributions of the final states will also be estimated, according to the
    *                      second theorem.
    * @return
    */
  def computeWTDists(
                      fsm: FSMInterface,
                      horizon: Int,
                      finalsEnabled: Boolean
                    ): scala.collection.immutable.Map[Int, WtDistribution] = {
    require(horizon > 0)
    val nn1c = buildNN1C(horizon)
    val isl = fsm.getNonFinals.toList
    // Improbable states should not produce forecasts.
    // The distribution will be wrong, giving 100% prob to reach a final state in the next transition.
    // So, for now, just let them have an empty distribution.
    //TODO: better to completely remove improbable states from the matrix.
    val wtsNonFinals = isl.map(
      x => if (isStateImprobable(x)) WtDistribution() else WtDistribution(getNonFinalWTDistribution(x, nn1c, horizon))
    )
    val wtMapNonFinals = isl.zip(wtsNonFinals).toMap
    var wtMap: scala.collection.immutable.Map[Int, WtDistribution] = wtMapNonFinals
    if (finalsEnabled) {
      val f1 = buildF1
      val FnNn2IN1 = buildFnNn2IN1(horizon, nn1c)
      val finals = fsm.getFinals.toList
      val wtsFinals = finals.map(
        x => if (isStateImprobable(x)) WtDistribution() else WtDistribution(getFinalWTDistribution(x, f1, FnNn2IN1, horizon))
      )
      val wtMapFinals = finals.zip(wtsFinals).toMap
      wtMap = wtMap ++ wtMapFinals
    }
    wtMap
  }

  /**
    * Checks whether a given FSM state is improbable to leave.
    *
    * @param state The given FSM state.
    * @return True if state improbable.
    */
  private def isStateImprobable(state: Int): Boolean = {
    val row = state2Row(state)
    var improbable = true
    for (col <- 0 until matrix.cols) {
      if (matrix(row, col) != 0.0) improbable = false
    }
    improbable
  }

  /**
    * @return F * 1
    */
  private def buildF1: DenseVector[Double] = {
    val size = matrix.rows
    val startOfFinals = size - absorbingNo
    val f = matrix(startOfFinals until size, startOfFinals until size)
    val onesv = DenseVector.ones[Double](absorbingNo)
    val f1 = f * onesv
    f1
  }

  /**
    * Estimates FN Nn−2(I − N )1 for all the horizon points.
    *
    * @param horizon The horizon.
    * @param Nn1IN1 The vectors Nn−1(I − N )1.
    * @return The vectors FN Nn−2(I − N )1.
    */
  private def buildFnNn2IN1(
                             horizon: Int,
                             Nn1IN1: mutable.Map[Int, DenseVector[Double]]
                           ): mutable.Map[Int, DenseVector[Double]] = {
    val FnNn2IN1 = mutable.Map[Int, DenseVector[Double]]()
    val size = matrix.rows
    val startOfFinals = size - absorbingNo
    val fn = matrix(startOfFinals until size, 0 until size - absorbingNo)
    for (h <- 2 to horizon) {
      val thisFnNn2IN1 = fn * Nn1IN1(h - 1)
      FnNn2IN1 += (h -> thisFnNn2IN1)
    }
    FnNn2IN1
  }

  //TODO: you can use diagonalization for efficient power computing (if n distinct eigenvalues exist)
  // assumes finals gathered at the end of m
  /**
    * Estimates Nn−1(I − N )1.
    *
    * @param horizon The horizon.
    * @return The vectors Nn−1(I − N )1.
    */
  private def buildNN1C(horizon: Int): mutable.Map[Int, DenseVector[Double]] = {
    val nn1c = mutable.Map[Int, DenseVector[Double]]()
    val size = matrix.rows
    val n = matrix(0 until size - absorbingNo, 0 until size - absorbingNo)
    val i = DenseMatrix.eye[Double](size - absorbingNo)
    val in = i - n
    val onesv = DenseVector.ones[Double](size - absorbingNo)
    var nn1 = DenseMatrix.eye[Double](size - absorbingNo)
    for (h <- 1 to horizon) {
      if (h != 1) {
        nn1 = nn1 * n
      }
      val nn1in: DenseMatrix[Double] = nn1 * in
      val nn1in1 = nn1in * onesv
      nn1c += (h -> nn1in1)
    }
    nn1c
  }

  /**
    * Estimates the waiting-time distribution of a given final state.
    *
    * @param initialState The given final state.
    * @param F1 See relevant theorem.
    * @param FnNn2IN1 See relevant theorem.
    * @param horizon The horizon.
    * @return The waiting-time distribution.
    */
  private def getFinalWTDistribution(
                                      initialState: Int,
                                      F1: DenseVector[Double],
                                      FnNn2IN1: mutable.Map[Int, DenseVector[Double]],
                                      horizon: Int
                                    ): scala.collection.immutable.Map[Int, Double] = {
    val size = matrix.rows
    val nonFinalsNo = size - absorbingNo
    require(state2Row(initialState) >= nonFinalsNo)
    val wt = mutable.Map[Int, Double]()
    val ksi = new DenseMatrix[Double](1, absorbingNo)
    val col = state2Row(initialState)
    ksi(0, col - nonFinalsNo) = 1.0
    var pv = new DenseVector[Double](1)
    for (h <- 1 to horizon) {
      if (h == 1) pv = ksi * F1
      else pv = ksi * FnNn2IN1(h)
      wt += (h -> pv(0))
    }
    wt.toMap
  }

  /**
    * Estimates the waiting-time distribution of a given non-final state.
    *
    * @param initialState The given non-final state.
    * @param nn1c See relevant theorem.
    * @param horizon The horizon.
    * @return The waiting-time distribution.
    */
  private def getNonFinalWTDistribution(
                                         initialState: Int,
                                         nn1c: mutable.Map[Int, DenseVector[Double]],
                                         horizon: Int
                                       ): scala.collection.immutable.Map[Int, Double] = {
    val size = matrix.rows
    require(state2Row(initialState) < size - absorbingNo)
    val wt = mutable.Map[Int, Double]()
    val ksi = new DenseMatrix[Double](1, size - absorbingNo)
    val col = state2Row(initialState)
    ksi(0, col) = 1.0
    var pv = new DenseVector[Double](1)
    for (h <- 1 to horizon) {
      pv = ksi * nn1c(h)
      wt += (h -> pv(0))
    }
    wt.toMap
  }

  //TODO: If patterns in stream impossible, matrix might have rows with zero prob. Find a way to deal with that.
  // Temporary "fix": if row has prob sum 0, check if column also has prob sum 0
  // i.e., if impossible to leave state, it should be impossible to enter it in the first place.
  /**
    * Checks whether the matrix is consistent, i.e.,
    *   it is square
    *   each state must be improbable to leave
    *     in which case we check that it is also (almost) improbable to reach it
    *   or the sum of outgoing probabilities must be close to 1.0
    *
    * @return True if the matrix is consistent.
    */
  private def checkMatrix(): Boolean = {
      def rowSum(r: Int): Double = {
        var s = 0.0
        for (j <- 0 until matrix.rows) {
          s += matrix(r, j)
        }
        s
      }

      def colSum(c: Int): Double = {
        var s = 0.0
        for (i <- 0 until matrix.cols) {
          s += matrix(i, c)
        }
        s
      }

      def isConsistentProb(v: Double): Boolean = {
        val min = 1.0 - consistencyTolerance
        val max = 1.0 + consistencyTolerance
        v > min & v < max
      }

    var consistent = true
    // check that the matrix is square
    if (matrix.rows != matrix.cols) consistent = false
    for (i <- 0 until matrix.rows) {
      val rs = rowSum(i)
      if (rs == 0) { // either improbable to leave a state
        val cs = colSum(i)
        // in which case check that it is also (almost) improbable to reach it
        if (cs > consistencyTolerance) {
          consistent = false
          throw new IllegalArgumentException("Inconsistent row/prob and col/prob " + i + "/" + rs + "/" + cs + " in \n" + matrix)
        }
      } else if (!isConsistentProb(rs)) {
        // or the sum of outgoing probabilities must be close to 1.0
        consistent = false
        throw new IllegalArgumentException("Inconsistent row/prob " + i + "/" + rs + " in \n" + matrix)
      }
    }
    consistent
  }

  /**
    * Serializes and writes the Markov chain to a file.
    *
    * @param fn The path to the file.
    */
  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
