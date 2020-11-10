package model.markov

import breeze.linalg.DenseMatrix
import fsm.FSMInterface

class MarkovChainFactory {

}

/**
  * Factory for constructing Markov chains.
  */
object MarkovChainFactory {

  /**
    * Constructs a Markov chain from a FSM and a set of conditional probabilities.
    *
    * @param fsm The FSM whose structure will guide the construction of the Markov chain.
    *            The states of the FSM become Markov chain states.
    * @param probs A set of conditional probabilities with which the transition matrix will be filled.
    * @return The Markov chain.
    */
  def buildMC(
               fsm: FSMInterface,
               probs: TransitionProbs
             ): MarkovChain = MarkovChain(fsm, probs)

  /**
    * Constructs a Markov chain from a matrix, a mapping of FSM to Markov states and the number of final states.
    *
    * @param matrix The transition matrix.
    * @param state2Row The mapping of FSM to Markov states.
    * @param absorbingNo The number of final states.
    * @return The Markov chain.
    */
  def buildMC(
               matrix: DenseMatrix[Double],
               state2Row: Map[Int, Int],
               absorbingNo: Int
             ): MarkovChain = {
    MarkovChain(matrix, state2Row, absorbingNo)
  }
}
