package fsm

import breeze.linalg.DenseMatrix
import model.markov.TransitionProbs
import stream.GenericEvent
import model.waitingTime.WtDistribution

/**
  * This is the main interface that should be implemented by every automaton class.
  */
trait FSMInterface {
  // For each state, the map contains the distance of the shortest path from the state to a final state.
  // Distance is calculate simply as the path's length (number of hops).
  // Estimated by calling fsm.FSMInterface.estimateRemainingPercentage.
  var shortestPathDistances: Map[Int, Int] = Map.empty
  // For each state, the map contains the expected distance from the state to a final state.
  // The expected distance is the expected value of the waiting-time distribution. We need to provide the waiting-time
  // distributions to estimate it. This map cannot be estimated solely by the FSM's structure.
  // Estimated by calling fsm.FSMInterface.estimateRemainingPercentage(wtds: Map[Int, WtDistribution]).
  var expectedPathLengths: Map[Int, Double] = Map.empty
  // For each state, the map contains the percentage of the FSM's "process" until completion. The remaining percentage
  // of a final state is 0%. since the process is already completed. The remaining percentage of the start state is
  // (usually) 100%.
  // Estimated by calling either fsm.FSMInterface.estimateRemainingPercentage or
  // fsm.FSMInterface.estimateRemainingPercentage(wtds: Map[Int, WtDistribution]).
  var remainingPercentage: Map[Int, Double] = Map.empty

  // All of the above maps (basically remainingPercentage) are used in order to establish checkpoints for forecasting.
  // See engine.ERFEngine. Not needed for recognition.
  // You do not want to estimate these maps by default during object construction,
  // since this estimation might be expensive. Use provided methods when needed.

  /**
    * @return The states of the FSM.
    */
  def getStates: Set[Int]

  /**
    * @return The start state.
    */
  def getStartId: Int

  /**
    * @return The final states.
    */
  def getFinals: Set[Int]

  /**
    * Returns true if the given state is a final one.
    *
    * @param state The state to check,
    * @return True if state is final.
    */
  def isFinal(state: Int): Boolean = getFinals.contains(state)

  /**
    * @return The non-final states.
    */
  def getNonFinals: Set[Int] = getStates &~ getFinals

  /**
    * @return The total number of states.
    */
  def size: Int = getStates.size

  /**
    * @return The total number of final states.
    */
  def getFinalsNo: Int = getFinals.size

  /**
    * Each FSM has a unique ID.
    *
    * @return The FSM's ID.
    */
  def getId: Int

  /**
    * Whenever a new event contains a new value for the FSM's partition attribute, a new run is spawned.
    *
    * @return The partition attribute
    */
  def getPartitionAttribute: String

  /**
    * Given a (current) state of the FSM and a new input event, find the next state.
    *
    * @param currentState The FSM's current state.
    * @param event The new input event.
    * @return The next state that the FSM moves to.
    */
  def getNextState(
                    currentState: Int,
                    event: GenericEvent
                  ): Int

  /**
    * Serializes the FSM and writes it to a file.
    *
    * @param fn The path to the file.
    */
  def write2File(fn: String): Unit

  /**
    * Builds a map from states of the FSM to rows of a Markov chain. Gathers all final states to the end of matrix.
    *
    * @return A map from states of the FSM to rows of a Markov chain, where all final states are at the end of the
    *         matrix.
    */
  def buildState2Row: Map[Int, Int] = {
    val finals = getFinals
    val (f, nf) = getStates.partition(finals.contains)
    // all non-final states reside at the first rows of the matrix
    val nfm = nf.zipWithIndex.map{ case (s, r) => s -> r }.toMap
    // all finals at the end of the matrix
    val fm = f.zipWithIndex.map{ case (s, i) => s -> (i + nfm.size) }.toMap
    nfm ++ fm
  }

  /**
    * Creates a Markov chain transition matrix from a set of conditional probabilities.
    * Used for testing, only by fsm.DFAInterface. For other FSMs, just creates a dummy matrix.
    *
    * @param probs The set of conditional probabilities.
    * @return A transition matrix corresponding to the given conditional probabilities.
    */
  def buildTransitionMatrix(probs: TransitionProbs): DenseMatrix[Double]

  /**
    * Estimates the remaining percentages via the expected distances.
    * Requires the waiting-time distributions of the states to be given.
    *
    * @param wtds The waiting-time distributions from which we can calculate the expected distance of every state.
    * @return The remaining percentage of every state.
    */
  def estimateRemainingPercentage(wtds: Map[Int, WtDistribution]): Map[Int, Double] = {
    // first find all expected distances
    val expectedPaths = findExpectedPathLengths(wtds)
    // then find the maximum
    val maxExpectedPath = expectedPaths.values.max
    // then just divide every distance by the maximum
    val percentage = expectedPaths.mapValues(x => x / maxExpectedPath)
    // set expectedPathLengths and remainingPercentage before returning remainingPercentage
    expectedPathLengths = expectedPaths
    remainingPercentage = percentage
    remainingPercentage
  }

  /**
    * Finds the expected distances of all states, i.e., the expected length of paths leading to a final state.
    *
    * @param wtds The waiting-time distributions from which we can calculate the expected distance of every state.
    * @return The expected distances for all states.
    */
  private def findExpectedPathLengths(wtds: Map[Int, WtDistribution]): Map[Int, Double] = {
    val states = getStates
    require(wtds.keySet.forall(x => states.contains(x)))
    val lengths = states.map(s => (s, {
      if (wtds.contains(s)) wtds(s).getExpectedValue
      else -1.0
    })).toMap
    lengths
  }

  /**
    * Estimates the remaining percentages via the shortest paths leading to final states.
    *
    * @return The remaining percentage of every state.
    */
  def estimateRemainingPercentage: Map[Int, Double] = {
    // first find all shortest path lengths.
    val shortestPaths = findShortestPathDistances
    // then find the maximum shortest path
    val maxShortestPath = shortestPaths.values.max
    // then just divide by this maximum
    val percentage = shortestPaths.mapValues(x => x.toDouble / maxShortestPath)
    shortestPathDistances = shortestPaths
    remainingPercentage = percentage
    remainingPercentage
  }

  /**
    * For each state of the FSM, find the length of the shortest path leading to a final state.
    *
    * @return a Map with all the states accompanied by the length of the shortest path to a final state.
    */
  def findShortestPathDistances: Map[Int, Int] = {
    val finals = getFinals
    // Assume, by default, that shortest path to a final from a final is 0.
    val finalsDistances: Map[Int, Int] = finals.map(f => (f, 0)).toMap
    val nonFinals = getNonFinals
    val shortestPaths = findShortestPathDistancesAux(nonFinals, finals, 0, finalsDistances)
    shortestPaths
  }

  /**
    * Recursive, auxiliary function to find lengths of shortest paths to a final.
    * The idea is the following: We start with the finals whose shortest path has zero length.
    * Then, we scan all the other states (remaining) and try to find which of those connect to the finals.
    * These states (S1) will have a shortest path of length 1. We repeat the process. We remove S1 from the
    * remaining states and set S1 as a frontier. We try to find which of the remaining states (all states minus
    * S1 and finals) are connected to S1. These states (S2) will have a shortest path of length 2. The process
    * is recursively repeated until the set of remaining states is empty.
    *
    * @param remaining The states that remain to be checked.
    * @param frontier The states last checked. They all have the same distance.
    * @param currentDistance The distance of the states in frontier.
    * @param tempResult Accumulator map that gathers all shortest length found so far.
    * @return Lengths of shortest paths to a final
    */
  @scala.annotation.tailrec
  private def findShortestPathDistancesAux(
                                            remaining: Set[Int],
                                            frontier: Set[Int],
                                            currentDistance: Int,
                                            tempResult: Map[Int, Int]
                                          ): Map[Int, Int] = {
    if (remaining.isEmpty) tempResult
    else {
      val newDistance = currentDistance + 1
      val connectedToFrontier = remaining.filter(r => frontier.exists(f => connected(r, f)))
      // It is possible to have states that never reach a final state (e.g., dead states).
      // For these, just set -1 as their distance.
      if (connectedToFrontier.isEmpty) {
        val newResult = remaining.map(s => (s, -1)).toMap
        newResult ++ tempResult
      } else {
        val newResult = connectedToFrontier.map(x => (x, newDistance)).toMap
        val newRemaining = remaining &~ connectedToFrontier
        findShortestPathDistancesAux(newRemaining, connectedToFrontier, newDistance, newResult ++ tempResult)
      }
    }
  }

  /**
    * Checks whether there is a transition from a given state to another.
    *
    * @param from The candidate source state.
    * @param to The candidate target state.
    * @return Return true if there is indeed a transition.
    */
  def connected(
                 from: Int,
                 to: Int
               ): Boolean

  /**
    * For each state of the FSM, find the length of the shortest path leading to a final state.
    * Same as fsm.FSMInterface#shortestWalkDistances() but exhaustively finds all paths from a state to a final
    * and then keeps the shortest.
    *
    * To be used ONLY for testing. Highly inefficient.
    *
    * @return a Map with all the states accompanied by the length of the shortest path to a final state.
    */
  def findShortestPathDistancesPaths: Map[Int, Int] = {
    val finals = getFinals
    val finalsDistances: Map[Int, Int] = finals.map(f => (f, 0)).toMap
    val nonFinals = getNonFinals
    val nonFinalsDistances = nonFinals.map(n => (n, findShortestDistancePath(n))).toMap
    nonFinalsDistances ++ finalsDistances
  }

  private def findShortestDistancePath(from: Int): Int = {
    val paths = findSathsToFinals(from)
    if (paths.isEmpty) -1
    else paths.map(p => p.length - 1).min
  }

  private def findSathsToFinals(from: Int): Set[List[Int]] = {
    val paths = findPathsToFinalsAux(Set(List(from)))
    paths
  }

  @scala.annotation.tailrec
  private def findPathsToFinalsAux(partialPaths: Set[List[Int]]): Set[List[Int]] = {
    if (partialPaths.nonEmpty & partialPaths.exists(p => !isPathToFinal(p))) {
      val newPaths = partialPaths.flatMap(p => expandPath(p))
      findPathsToFinalsAux(newPaths)
    } else partialPaths
  }

  private def isPathToFinal(path: List[Int]): Boolean = {
    getFinals.contains(path.head)
  }

  private def expandPath(partialPath: List[Int]): Set[List[Int]] = {
    if (isPathToFinal(partialPath)) Set(partialPath)
    else {
      val candidates = getStates &~ partialPath.toSet
      val connectedCandidates = candidates.filter(s => connected(partialPath.head, s))
      connectedCandidates.map(c => c :: partialPath)
    }
  }

  //TODO: remove method
  //def getPattern: String
  //TODO: remove method
  //def getOutput(state: Int): Set[String]

}
