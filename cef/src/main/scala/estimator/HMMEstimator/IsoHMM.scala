package estimator.HMMEstimator

import model.vmm.mapper.Isomorphism
import smile.sequence.HMM

/**
  *
  * A class for representing a HMM corresponding to a symbolic automaton.
  *
  * @param hmm The HMM.
  * @param iso Each label is again an Int, corresponding to a minterm. We get a mapping from minterms to ints through an
  *            isomorphism.
  * @param stateEncoding Each observation is an Int corresponding to a FSM state. However, we do not use the state
  *                      number directly. We create a mapping from the states to a list of increasing ints, with
  *                      stateEncoding.
  */
class IsoHMM private[HMMEstimator] (
                                     val hmm: HMM[Int],
                                     val iso: Isomorphism,
                                     val stateEncoding: Map[Int, Int]
                                   ) {

  override def toString: String = {
    "ISO: " + iso.toString + "\n" +
      "HMM: " + hmm.toString + "\n" +
      "State Encoding: " + stateEncoding.toString()
  }
}
