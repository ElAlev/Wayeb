package stream.array

import model.vmm.pst.psa.ProbSuffixAutomaton

/**
  * Creates a random event stream from a probabilistic suffix automaton.
  *
  * @param psa The probabilistic suffix automaton to act as event generator.
  * @param size The size of the stream.
  */
class PSAStream private[stream] (
                                  psa: ProbSuffixAutomaton,
                                  size: Int
                                ) extends EventStreamI {

  override def generateStream(): EventStream = {
    val (eventStream, _) = psa.generateStream(size)
    eventStream
  }

}
