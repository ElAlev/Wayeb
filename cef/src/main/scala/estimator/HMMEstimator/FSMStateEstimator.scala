package estimator.HMMEstimator

import estimator.RunEstimator
import fsm.SDFAInterface
import fsm.runtime.RunMessage
import model.vmm.Symbol
import model.vmm.mapper.Isomorphism
import smile.sequence.HMM
import model.vmm.pst.BufferBank
import ui.ConfigUtils

object FSMStateEstimator {
  /**
    * Constructor for FSM state estimator.
    *
    * @param fsm The FSM for which we want to train a HMM.
    * @return The estimator.
    */
  def apply(fsm: SDFAInterface): FSMStateEstimator = new FSMStateEstimator(fsm)
}

/**
  * Builds a prediction model based on (not a direct implementation)
  * Suraj Pandey, Surya Nepal, and Shiping Chen. A test-bed for the evaluation of business
  * process prediction techniques. In CollaborateCom, pages 382–391. ICST / IEEE, 2011.
  *
  * For a FSM, we train a HMM. The observed variable of the HMM is the state of the FMS, i.e., a sequence of
  * observations for the HMM is constructed by getting the sequence of states visited by the FSM. The hidden variable is
  * the minterm that evaluates to true, i.e., a sequence of labels is constructed by getting the sequence of minterms
  * that evaluated to true.
  *
  * @param fsm The FSM for which we want to train a HMM.
  */
class FSMStateEstimator private (fsm: SDFAInterface) extends RunEstimator with Serializable {
  // The list of observation sequences. Each observation is an Int corresponding to a FSM state. However, we do not use
  // the state number directly. We create a mapping from the states to a list of increasing ints. See stateEncoding.
  private var observations: List[List[Int]] = List.empty
  private val fsmStates = fsm.getStates.toList
  private val stateEncoding: Map[Int, Int] = fsmStates.zipWithIndex.toMap

  // The list of label sequences. Each label is again an Int, corresponding to a minterm. We get a mapping from minterms
  // to ints through an isomorphism.
  private var labels: List[List[Int]] = List.empty
  private val sentences = fsm.sdfa.getSentences.toList
  private val symbolsList = sentences.indices.map(x => Symbol(x)).toList
  private val iso: Isomorphism = Isomorphism(sentences, symbolsList)

  // We need to decide how deep we want to unroll the HMM. Since we have a stream, we cannot unroll it all the way back
  // to the start of the stream. As an alternative, from each state we find the path (shortest, since there might
  // multiple paths) that leads to a final state and then keep the longest one.
  private val longestShortestPath = fsm.findShortestPathDistances.values.max
  private val obsBuffers = new BufferBank(longestShortestPath + 1)
  private val labBuffers = new BufferBank(longestShortestPath + 1)

  private var hmm: HMM[Int] = _

  private var runningObservations: Map[String, List[Int]] = Map.empty
  private var runningLabels: Map[String, List[Int]] = Map.empty

  override def newEventProcessed(rm: RunMessage): Unit = {
    if (!rm.isReset) {
      //startBasedLearning(rm)
      val partitionAttribute = fsm.getPartitionAttribute
      finalsBasedLearning(rm, partitionAttribute)
    }
  }

  /**
    * Constructs the list of observation and label sequences.
    * Every time we detect a complex event (we are in a final state), we add the observation and label sequences up to
    * the current event to their respective lists.
    *
    * @param rm The message from the automaton run.
    * @param partitionAttribute The partition attribute.
    */
  private def finalsBasedLearning(
                                   rm: RunMessage,
                                   partitionAttribute: String
                                 ): Unit = {
    // get the Int for the state through the state encoding
    val observedState = Symbol(stateEncoding(rm.currentState))
    val event = rm.lastEvent
    // get the Int for the minterm through the isomorphism
    val label = iso.evaluate(event)
    val av =
      if (partitionAttribute.equalsIgnoreCase(ConfigUtils.singlePartitionVal)) ConfigUtils.singlePartitionVal
      else event.getValueOf(partitionAttribute).toString
    obsBuffers.push(av, observedState)
    labBuffers.push(av, label)
    if (rm.fmDetected) {
      val bufferedObservation = obsBuffers.pop(av)
      val bufferedLabel = labBuffers.pop(av)
      observations = bufferedObservation.map(x => x.value) :: observations
      labels = bufferedLabel.map(x => x.value) :: labels
    }
  }

  /**
    * An alternative to constructing the list of observation and label sequences. As we consume the stream,
    * we add a new observation every time the FSM is not in its start state and a new observation sequence every time
    * the FSM is in a start state. A FSM may be in its start state for many events before it starts proceeding and we
    * do not want all these trivial observations. We essentially "split" the stream using the start states as
    * separators. This can lead to long observation/label sequences though.
    *
    * @param rm The message from the automaton run.
    * @param partitionAttribute The partition attribute.
    */
  private def startBasedLearning(
                                  rm: RunMessage,
                                  partitionAttribute: String
                                ): Unit = {
    val observedState = rm.currentState
    val event = rm.lastEvent
    val label = iso.evaluate(event).value
    val av =
      if (partitionAttribute.equalsIgnoreCase(ConfigUtils.singlePartitionVal)) ConfigUtils.singlePartitionVal
      else event.getValueOf(partitionAttribute).toString
    if (runningObservations.contains(av)) {
      require(runningLabels.contains(av))
      val runningObservation = runningObservations(av)
      if (observedState != fsm.getStartId) {
        // if the state is not the start state, then add the observation to the sequence
        val newObs = observedState :: runningObservation
        val runningLabel = runningLabels(av)
        require(runningObservation.length == runningLabel.length)
        val newLabel = label :: runningLabel
        runningObservations += (av -> newObs)
        runningLabels += (av -> newLabel)
      } else {
        // If it is the start state,
        if (runningObservation.head != fsm.getStartId) {
          // and it's the first time we see a start state, we store the running observation
          val runningLabel = runningLabels(av)
          require(runningObservation.length == runningLabel.length)
          observations = runningObservation :: observations
          labels = runningLabel :: labels
        }
        // also clear the running sequecnes by adding just the observation/label for this start state
        val newObs = List(observedState)
        val newLabel = List(label)
        runningObservations += (av -> newObs)
        runningLabels += (av -> newLabel)
      }
    } else {
      val newObs = List(observedState)
      val newLabel = List(label)
      runningObservations += (av -> newObs)
      runningLabels += (av -> newLabel)
    }
  }

  /**
    * After constructing the list of observation and label sequences, we can train the HMM.
    */
  override def estimate(): Unit = {
    val obsArray = observations.map(x => x.reverse).map(o => o.toArray).toArray
    val labArray = labels.map(x => x.reverse).map(l => l.toArray).toArray
    hmm = new HMM[Int](obsArray, labArray)
  }

  override def shutdown(): Unit = {

  }

  def getHMM: IsoHMM = new IsoHMM(hmm, iso, stateEncoding)

}
