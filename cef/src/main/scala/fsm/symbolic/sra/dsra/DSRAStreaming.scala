package fsm.symbolic.sra.dsra

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.Valuation
import fsm.symbolic.sra.{Configuration, SRAGuard, SRATransition}
import model.vmm.Symbol
import stream.GenericEvent
import utils.SetUtils

import java.io.{FileOutputStream, ObjectOutputStream}

object DSRAStreaming extends LazyLogging {
  /**
   * Constructor for streaming DSRA.
   *
   * @param dsra   The original DSRA.
   * @param window The DSRA's window.
   * @return       A streaming DSRA.
   */
  def apply(
             dsra: DSRA,
             window: Int
           ): DSRAStreaming = {
    logger.debug("Renaming")
    val renamedDSRA = renameStates(dsra)
    logger.debug("Building streaming DSRA")
    new DSRAStreaming(renamedDSRA, window)
  }

  /**
   * Renames all the states of a given DSRA so that their ids start from 0. Useful to avoid having states with id -1.
   *
   * @param dsra The original DSRA.
   * @return     The renamed DSRA.
   */
  private def renameStates(dsra: DSRA): DSRA = {
    val originalStatesIds = dsra.states.keySet.toList
    val newStatesIds = originalStatesIds.indices.toList
    val orig2new = originalStatesIds.zip(newStatesIds).toMap
    val newStates = newStatesIds.map(s => (s, DSRAState(s))).toMap
    val newTransitions = dsra.transitions.map(t => {
      val newSource: Int = orig2new(t.source)
      val newTarget: Int = orig2new(t.target)
      val newGuard: SRAGuard = t.guard.asInstanceOf[SRAGuard]
      val newOutput: TransitionOutput = t.output
      val newWriteRegs: Set[String] = t.writeRegisters
      SRATransition(newSource,newTarget,newGuard,newOutput,newWriteRegs)
    })
    val newStartId = orig2new(dsra.start)
    val newFinalsIds = dsra.finals.map(s => orig2new(s))
    val newDSRA = DSRA(newStates,newTransitions,newStartId,newFinalsIds)
    newDSRA
  }
}

/**
 * Class representing a streaming version of DSRA.
 * A streaming DSRA is essentially composed of multiple copies of the original DSRA.
 * The number of copies is equal to the length of the DSRA's window.
 * The first copy starts processing from the first event (and then again at the  (window+1)th event etc.), the second
 * copy starts at the second event (and then again at the  (window+2)th event etc.), etc.
 * Essentially, the use of copies simulates the use of a sliding window of length window.
 * With one DSRA, we could use this sliding window to feed its contents to the DSRA and wait for a reply (are the
 * contents accepted or not?).
 * Reminder: Windowed DSRA cannot work on unbounded streams, thus we cannot just feed the
 * whole stream to a DSRA and expect it to produce matches.
 * By using multiple copies of the DSRA, we are then forced to have hyper-states, since each copy might, at any given
 * moment, be in a different state. A set of sub-states (states of the copied sub-automata) constitutes a hyper-state.
 *
 * @param dsra    The original DSRA.
 * @param window  The DSRA's window.
 */
class DSRAStreaming private[dsra] (
                                    dsra: DSRA,
                                    val window: Int
                                  ) extends Serializable with LazyLogging {
  // Construct the hyper-states.
  logger.debug("Constructing hyper-states")
  private val statesLists: List[List[Int]] = (1 to window).map(w => dsra.states.keySet.toList).toList
  private val statesCart: List[List[Int]] = SetUtils.cartesian(statesLists)
  val hyperStates: List[Int] = statesCart.indices.toList
  // Construct mappings from/to hyper-states to/from sub-states.
  logger.debug("Constructing mappings from/to hyper-states to/from sub-states")
  private val hyper2subStates: Map[Int, List[Int]] = hyperStates.zip(statesCart).toMap
  private val sub2hyperStates: Map[List[Int], Int] = statesCart.zip(hyperStates).toMap
  // Find the final hyper-states, i.e., those hyper-states which have a member sub-state which is in itself a final state.
  logger.debug("Finding the final hyper-states")
  val finals: Set[Int] = hyperStates.filter(h => {
    val sub = hyper2subStates(h)
    sub.exists(s => dsra.finals.contains(s))
  } ).toSet
  // The start hyper-state is the set of all the start sub-states.
  val start: Int = sub2hyperStates((1 to window).map(w => dsra.start).toList)

  val transitions: List[SRATransition] = dsra.transitions

  //private val symbolsList: List[Symbol] = Symbol() :: (1 to transitions.size).map(x => Symbol(x)).toList
  //private val dummyTransition: SRATransition = SRATransition(dsra.start, dsra.start, SRAGuard(SentenceConstructor.getNewTrueSentence))
  //private val t2s: Map[SRATransition, Symbol] = (dummyTransition :: transitions).zip(symbolsList).toMap
  //private val symbolsMultiLists: List[List[Symbol]] = (1 to window).map(w => symbolsList).toList
  //private val symbolsCart: List[List[Symbol]] = SetUtils.cartesian(symbolsMultiLists)
  //val hyperSymbols: List[Symbol] = (1 to symbolsCart.size).map(x => Symbol(x)).toList
  //private val hyper2subSymbols:  Map[Symbol, List[Symbol]] = hyperSymbols.zip(symbolsCart).toMap
  //private val sub2hyperSymbols: Map[List[Symbol], Symbol] = symbolsCart.zip(hyperSymbols).toMap

  /**
   * Produces a set of new successor configurations from a given configuration and a given input event.
   * The returned set should be a singleton.
   *
   * @param fromConfiguration The given configuration.
   * @param withEvent         The given event.
   * @return                  A set of new successor configurations (should be a singleton).
   */
  def yieldsSuccessorConfig(
                             fromConfiguration: Configuration,
                             withEvent: GenericEvent
                           ): Set[Configuration] = {
    // First find the next configurations from all of the sub-automata
    val subConfs = yieldsSuccessorConfigs(fromConfiguration, withEvent)
    // Convert the next sub-states to a hyper-state
    val hyperState = sub2hyperStates(subConfs.map(c => c.stateId))
    // All sub-valuations are properly renamed and gathered to a new valuation map
    val hyperValuationMap = subConfs.zipWithIndex.map(c => renameValuation(c._1.valuation,c._2).v).foldLeft(Map.empty[String, GenericEvent]) {
      (acc, x) => acc ++ x
    }
    val hyperValuation = Valuation(hyperValuationMap)
    // Construct the new hyper-symbol from the sub-states.
    val hyperSymbol = Symbol(sub2hyperStates(subConfs.map(c => c.symbol.value)))
    val hyperConf = Configuration(hyperState, hyperValuation, fromConfiguration.index + 1, hyperSymbol)
    Set(hyperConf)
  }

  /**
   * Renames the registers of a valuation so that they have a suffix.
   * For example, if we're dealing with the 3rd copy/sub-automaton, then register r2 will be renamed as r2.3.
   *
   * @param valuation The valuation to be renamed.
   * @param clone     The copy number.
   * @return          A valuation with renamed registers.
   */
  private def renameValuation(
                               valuation: Valuation,
                               clone: Int
                             ): Valuation = {
    val newMap = valuation.v.map(r => (r._1+"."+clone, r._2))
    Valuation(newMap)
  }

  /**
   * Produces the next configurations of all sub-automata from a given hyper configuration and an event.
   *
   * @param fromHyperConf The given hyper configuration.
   * @param withEvent     The given event.
   * @return              The next configurations from the sub-automata, provided as a list.
   */
  private def yieldsSuccessorConfigs(
                                      fromHyperConf: Configuration,
                                      withEvent: GenericEvent
                                    ): List[Configuration] = {
    val index = fromHyperConf.index
    val nextConfs = (0 until window).map(d => {
      // first, we need to find the sub-state
      val candidateSubState = hyper2subStates(fromHyperConf.stateId)(d)
      // if the current sub-state has no outgoing transitions, we need to reset the automaton
      val subState = if (dsra.hasOutgoing(candidateSubState)) candidateSubState else dsra.start
      // now, we need to construct the proper valuation, by removing the suffices from the registers
      val subValuationRegisters = fromHyperConf.valuation.v.filter(r => {
        val splitReg = r._1.split("\\.")
        splitReg(1).equalsIgnoreCase(d.toString)
      }).map(fr => {
        val splitReg = fr._1.split("\\.")
        (splitReg(0), fr._2)
      })
      val subValuation = Valuation(subValuationRegisters)
      val subConf = Configuration(subState, subValuation)
      // We need to decide whether the current copy may process the event.
      // First, enough events must have elapse so that the copy has started (e.g., the third copy cannot process
      // the first and second event), This is ensured with the first condition (d <= index).
      // Second, the copy can process the event if it is in a non-start state (it cannot be in a terminal state
      // without outgoing transitions, this case has been handled above).
      // Third, if it is in a start state, then the stream index must be at the proper position, e.g. the first copy can
      // start processing at the first event, the (window+1)th event, etc. This is ensured with the third condition
      // index % window == d
      if ( (d <= index) & ((subState != dsra.start) | (index % window == d))) {
        val nextConfs = yieldsSuccessorConfigWithSymbol(subConf, withEvent)
        // If we were in a terminal state and cannot move to a new state, reset the automaton.
        // This should not normally occur. Added here for extra safety.
        if (nextConfs.nonEmpty) nextConfs.head
        else Configuration(dsra.start, Valuation(), fromHyperConf.index + 1, Symbol(dsra.start))
      }
        // If the current copy cannot process the current event, then stay in the same configuration and just increase
        // the index.
      else {
        Configuration(subConf.stateId, subConf.valuation, fromHyperConf.index + 1, Symbol(subConf.stateId))
      }
    }).toList
    nextConfs
  }

  /**
   * Finds the next configurations of a sub-automaton from another given configuration and a given event.
   * Also adds a symbol to the next configurations. Note that in this case we set the next symbol to be the id of the
   * next state.
   *
   * @param fromSubConfiguration  The given sub-configuration.
   * @param withEvent             The given event.
   * @return                      A set of next sub-configurations. Should be a singleton.
   */
  private def yieldsSuccessorConfigWithSymbol(
                                               fromSubConfiguration: Configuration,
                                               withEvent: GenericEvent
                                             ): Set[Configuration] = {
    val fromStateId = fromSubConfiguration.stateId
    require(dsra.states.contains(fromStateId), "DSRA\n" + this.toString + "\n does not have state: " + fromStateId)
    val fromValuation = fromSubConfiguration.valuation
    val relevantTransitions = transitions.filter(t => t.source == fromStateId)
    val enabledTransitionsValuations = relevantTransitions.
      filter(!_.isEpsilon).
      map(t => {
        val newConf = t.yields(withEvent, fromValuation)
        (newConf._1, newConf._2, t.target, Symbol(t.target))
      }).
      filter(_._1)
    val result = enabledTransitionsValuations.map(t => Configuration(t._3, t._2, fromSubConfiguration.index+1, t._4)).toSet
    result
  }

  /**
   * Finds the state we can reach from another given state and a given symbol.
   * This is basically a dummy function, since the next symbol is the id of the next state. Thus, we just return the
   * value of the symbol.
   *
   * @param state  The given state.
   * @param symbol The given symbol. Must belong to the sets of symbols of the automaton.
   * @return       The id of the next state.
   */
  def getNextStateWithSymbol(
                              state: Int,
                              symbol: Symbol
                            ): Int = symbol.value


  /**
   * Determines whether a given hyper-state is connected to another given hyper-state.
   *
   * @param fromHyperState The source hyper-state.
   * @param toHyperState   The target hyper-state.
   * @return               True if the hyperstates are connected.
   */
  def connected(
                 fromHyperState: Int,
                 toHyperState: Int
               ): Boolean = {
    // First find all source and target substates.
    val fromSubStates = hyper2subStates(fromHyperState)
    val toSubStates = hyper2subStates(toHyperState)
    // Then, for each pair of substates, check whether they are connected.
    val subStatesPairs = fromSubStates.zip(toSubStates)
    subStatesPairs.forall(x => subConnected(x._1, x._2))
  }

  /**
   * Determines a given sub-state is connected to another given sub-state.
   * Two sub-states are connected iff:
   * Either a direct transition exists from the source to the target state.
   * Or there exists an implicit connection, i.e., the source state is a terminal state and the target state has an
   * incoming transition from the start state. These implicit connections are due to the resetting mechanism that
   * fsm.symbolic.sra.dsra.DSRAStreaming#yieldsSuccessorConfigs(fsm.symbolic.sra.Configuration, stream.GenericEvent)
   * has.
   *
   * @param fromSubState  The source sub-state.
   * @param toSubState    The target hyper-state.
   * @return              True if the sub-states are connected.
   */
  private def subConnected(
                            fromSubState: Int,
                            toSubState: Int
                          ): Boolean = {
    val directConnection = dsra.connected(fromSubState, toSubState)
    val implicitConnection = (!dsra.hasOutgoing(fromSubState) & dsra.connected(dsra.start, toSubState))
    directConnection | implicitConnection
  }

  /**
   * Determines whether a given hyper-state is final.
   *
   * @param hyperstate The given hyper-state.
   * @return True if the given hyper-state is final.
   */
  def isFinal(hyperstate: Int): Boolean = {
    require(hyperStates.contains(hyperstate))
    finals.contains(hyperstate)
  }

  /**
   * Determines whether a given hyper-state is the start state.
   *
   * @param hyperstate The given hyper-state.
   * @return True if the given hyper-state is the start state.
   */
  def isStart(hyperstate: Int): Boolean = {
    require(hyperStates.contains(hyperstate))
    hyperstate == start
  }

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

  override def toString: String = dsra.toString + "\nState mapping\n" + hyper2subStates

}
