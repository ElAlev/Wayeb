package estimator.MatrixEstimator

import java.io.{FileOutputStream, ObjectOutputStream}
import breeze.linalg.{*, DenseMatrix, sum}
import estimator.RunEstimator
import fsm.FSMInterface
import fsm.runtime.RunMessage

object MLEEstimator {
  /**
    * Constructor for MLE estimator.
    *
    * @param fsm The FSM interface for which we want to estimate a Markov chain.
    * @return The estimator.
    */
  def apply(fsm: FSMInterface): MLEEstimator = new MLEEstimator(fsm)
}

/**
  * Class to estimate the transition matrix of a FSM's Markov chain through maximum likelihood estimators.
  * See Elias Alevizos, Alexander Artikis, and George Paliouras. Event forecasting with pattern
  * markov chains. In DEBS, pages 146–157. ACM, 2017
  *
  * @param fsm The FSM interface for which we want to estimate a Markov chain.
  */
class MLEEstimator private (fsm: FSMInterface) extends RunEstimator with Serializable {
  // Counter matrix that will hold the number of transitions from a state i to a state j.
  private val n = DenseMatrix.zeros[Int](fsm.size, fsm.size)
  // Transition matrix to be estimated fron counter matrix.
  private val m = DenseMatrix.zeros[Double](fsm.size, fsm.size)
  // Build the map from states of the FSM to rows of the matrix.
  private val state2Row = fsm.buildState2Row
  private var ranking = Map[Int, Rank]()

  /**
    * Runs for every new event and modifies the counter matrix.
    *
    * @param rm Message from the automaton run than handled the event.
    */
  override def newEventProcessed(rm: RunMessage): Unit = {
    if (!rm.isReset) {
      // first find the rows corresponding to the previous and the current state of the FSM
      val from = state2Row(rm.previousState)
      val to = state2Row(rm.currentState)
      // and increase the counter of the entry for transitions from the previous to the current state
      n(from, to) = n(from, to) + 1
    }
  }

  override def shutdown(): Unit = {}

  /**
    * After every training event has been consumed, this method has to be called to estimate the transition matrix
    * from the counter matrix.
    */
  override def estimate(): Unit = {
    val rowSums = sum(n(*, ::)).t // TODO use the transpose vector (changed in breeze 1.0)
    for (r <- 0 until n.rows; c <- 0 until n.cols) {
      if (rowSums(r) != 0) m(r, c) = n(r, c) / rowSums(r).toDouble
      //if (m(r,c)==1.0) {
      //  println("1.0@" + r + "/" + c)
      //}
    }
  }

  def score: Double = {
    require(ranking.size == n.rows)
    var numerator = 0.0
    var denominator = 0.0
    for (r <- (0 until n.rows); c <- (0 until n.cols)) {
      denominator += n(r, c)
      numerator += n(r, c) * ranking(r).getRankFor(c)
    }
    numerator / denominator
  }

  def buildRanking(how: String): Unit = {
    for (r <- (0 until n.rows)) {
      val row = m(r, ::).t
      val tmpl = row.toArray.toList
      val tmpi = tmpl.zipWithIndex
      if (how.equalsIgnoreCase("modified")) {
        val par = mypartition(tmpi, tmpl.distinct.sorted)
        val rowRank = new Rank(myrank(par).reverse.toMap)
        ranking += (r -> rowRank)
      } else if (how.equalsIgnoreCase("standard")) {
        val par = mypartition(tmpi, tmpl.distinct.sorted.reverse)
        val rowRank = new Rank(myrank2(par).toMap)
        ranking += (r -> rowRank)
      } else throw new IllegalArgumentException("unrecognized ranking method")

    }
  }

  def cleanN(): Unit = {
    for (r <- (0 until n.rows); c <- (0 until n.cols)) {
      n(r, c) = 0
    }
  }

  def clean(): Unit = {
    for (r <- (0 until n.rows); c <- (0 until n.cols)) {
      n(r, c) = 0
      m(r, c) = 0.0
      ranking = Map[Int, Rank]()
    }
  }

  private def mypartition(
                           inds: List[(Double, Int)],
                           vals: List[Double]
                         ): List[List[Int]] = {
    vals match {
      case Nil => Nil
      case _ => inds.filter(x => x._1 == vals.head).map(x => x._2) :: mypartition(inds, vals.tail)
    }
  }

  private def myrank(ll: List[List[Int]]): List[(Int, List[Int])] = {
    val rank = ll.flatten.size
    ll match {
      case Nil => Nil
      case head :: tail => (rank, head) :: myrank(tail)
    }
  }

  def myrank2Aux(
                  ll: List[List[Int]],
                  seen: Int
                ): List[(Int, List[Int])] = {
    require(seen >= 0)
    ll match {
      case Nil => Nil
      case head :: tail => (seen + 1, head) :: myrank2Aux(tail, seen + head.size)
    }
  }

  def myrank2(ll: List[List[Int]]): List[(Int, List[Int])] = myrank2Aux(ll, 0)

  private class Rank(r: Map[Int, List[Int]]) {
    def getRanking: Map[Int, List[Int]] = r
    def getRankFor(j: Int): Int = {
      getRankForAux(j, r.toList)
    }
    @scala.annotation.tailrec
    private def getRankForAux(
                               j: Int,
                               rl: List[(Int, List[Int])]
                             ): Int = {
      rl match {
        case Nil => throw new IllegalArgumentException
        case head :: tail => if (head._2.contains(j)) head._1 else getRankForAux(j, tail)
      }
    }
    override def toString: String = r.toString()
  }

  def getTransitionMatrix: DenseMatrix[Double] = m

  def m2str: String = {
    var mstr = ""
    for (r <- 0 until m.rows; c <- 0 until m.cols) {
      if (m(r, c) != 0.0) mstr += r + "->" + c + ":" + m(r, c).toString + "\n"
    }
    mstr + "\n" + state2Row.toString()
  }

  def state2RowMapping: Map[Int, Int] = state2Row

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
