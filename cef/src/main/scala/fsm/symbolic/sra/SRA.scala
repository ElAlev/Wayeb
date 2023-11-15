package fsm.symbolic.sra

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.{Automaton, Valuation}
import stream.GenericEvent

/**
 * Abstract class representing Symbolic Register Automata.
 *
 * @param states The automaton states, as a map of state ids to states.
 * @param transitions The list of transitions.
 * @param start The id of the start state.
 * @param finals The set of ids of the final states.
 */
abstract class SRA(
                    states: Map[Int, SRAState],
                    transitions: List[SRATransition],
                    start: Int,
                    finals: Set[Int]
                  ) extends Automaton(states, transitions, start, finals) with LazyLogging {
  //require(!states.keySet.exists(s => s < 0))

  /**
   * Finds the states reached from the given state, with the given input event, taking into account epsilon
   * transitions. All epsilon transitions from the given state and from the states reached with the event must also be
   * included, if the automaton is non-deterministic. Valuation is assumed to be empty.
   *
   * @param stateId The id of the given state.
   * @param event The input event.
   * @return The states reached with the input event, also following any epsilon transitions.
   */
  override def getDeltaWithEpsilon(
                                    stateId: Int,
                                    event: GenericEvent
                                  ): Set[Int] = {
    val successorConfs = yieldsSuccessorConfigWithEpsilon(Configuration(stateId, Valuation()), event)
    successorConfs.map(_.stateId)
  }

  /**
   * Produces a set of new successor configurations from a given configuration and a given input event.
   * Assumes that epsilon transitions may be present. Thus, enclosure is also used.
   *
   * @param fromConf The given configuration.
   * @param withEvent The given input event.
   * @return A set of new successor configurations.
   */
  def yieldsSuccessorConfigWithEpsilon(
                                        fromConf: Configuration,
                                        withEvent: GenericEvent
                                      ): Set[Configuration] = {
    require(isConfigurationCompatible(fromConf), "Configuration provided is not compatible with SRA")
    val enclosure = enclose(fromConf.stateId)
    val delta = enclosure.foldLeft(Set.empty[Configuration]) {
      (acc, x) => acc ++ yieldsSuccessorConfigNoEpsilon(Configuration(x, fromConf.valuation), withEvent)
    }
    encloseConf(delta)
  }

  /**
   * Variant of enclosure where we start from a given set of configurations and try to see which new configurations
   * we can reach by following epsilon transitions.
   *
   * @param confs The given set of configurations.
   * @return The new set of configurations reached via epsilon transitions.
   */
  private def encloseConf(confs: Set[Configuration]): Set[Configuration] =
    confs.foldLeft(Set.empty[Configuration]) {
      (acc, x) => acc ++ enclose(x.stateId).map(e => Configuration(e,x.valuation))
    }

  /**
   * Determines if a string (i.e., list of events) is accepted by the SRA.
   * Assumes that the initial valuation is empty, i.e., all registers are empty.
   *
   * @param events The input events.
   * @return True if the SRA accepts the events.
   */
  override def accepts(events: List[GenericEvent]): Boolean = accepts(events, Valuation())

  /**
   * Determines if a string (i.e., list of events) is accepted by the SRA, starting from a given valuation.
   *
   * @param events The string.
   * @param initialValuation The initial valuation.
   * @return True if the string is accepted by the SRA when starting from the given initial valuation.
   */
  def accepts(
               events: List[GenericEvent],
               initialValuation: Valuation
             ): Boolean = {
    require(events.nonEmpty)
    val initialConf = Configuration(start, initialValuation)
    val reachedConfs = acceptsAux(events, Set(initialConf))
    reachedConfs.map(_.stateId).intersect(finals).nonEmpty
  }

  @scala.annotation.tailrec
  private def acceptsAux(
                          events: List[GenericEvent],
                          reached: Set[Configuration]
                        ): Set[Configuration] = {
    events match {
      case Nil => reached
      case head :: tail => {
        val newReached = reached.foldLeft(Set.empty[Configuration]){ (acc, x) => acc ++ yieldsSuccessorConfigWithEpsilon(x, head) }
        acceptsAux(tail, newReached)
      }
    }
  }

  /**
   * Produces a set of new successor configurations from a given configuration and a given input event.
   * Ignores any epsilon transitions.
   *
   * @param fromConfiguration The given configuration.
   * @param withEvent The given event.
   * @return A set of new successor configurations.
   */
  def yieldsSuccessorConfigNoEpsilon(
                                      fromConfiguration: Configuration,
                                      withEvent: GenericEvent
                                    ): Set[Configuration] = {
    val fromStateId = fromConfiguration.stateId
    require(states.contains(fromStateId), "SRA\n" + this.toString + "\n does not have state: " + fromStateId)
    val fromValuation = fromConfiguration.valuation
    val relevantTransitions = transitions.filter(t => t.source==fromStateId)
    val enabledTransitionsValuations = relevantTransitions.
      filter(!_.isEpsilon).
      map(t => {
        val newConf = t.yields(withEvent, fromValuation)
        (newConf._1, newConf._2, t.target, t.output)
      }).
      filter(_._1)
    val result = enabledTransitionsValuations.map(t => Configuration(t._3, t._2, fromConfiguration.index+1,t._4)).toSet
    result
  }

  /**
   * Produces a set of new successor configurations from a given configuration and a given input event.
   * Ignores any epsilon transitions.
   * Optimized version with arrays and while loops.
   *
   * @param fromConfiguration The given configuration.
   * @param withEvent         The given event.
   * @return An array of new successor configurations.
   */
  def yieldsSuccessorConfigNoEpsilonOpt(
                                         fromConfiguration: Configuration,
                                         withEvent: GenericEvent
                                       ): Array[Configuration] = {
    val fromStateId = fromConfiguration.stateId
    val fromValuation = fromConfiguration.valuation
    val relevantTransitions = transitionsMapAsArray(fromStateId)
    var i = 0
    val r: Array[Configuration] = new Array[Configuration](relevantTransitions.length)
    while (i < relevantTransitions.length) {
      val t = relevantTransitions(i).asInstanceOf[SRATransition]
      if (!t.isEpsilon) {
        val result = t.yields(withEvent, fromValuation)
        if (result._1) {
          val newConf = Configuration(t.target, result._2, fromConfiguration.index+1, t.output)
          r(i) = newConf
        }
      }
      i += 1
    }
    r
  }

  /**
   * Determines whether a given configuration is compatible with the SRA.
   * It is compatible if the configuration's state is also a SRA state.
   *
   * @param conf The given configuration.
   * @return True if the given configuration is valid.
   */
  private def isConfigurationCompatible(conf: Configuration): Boolean = {
    val isStateValid = states.contains(conf.stateId)
    //val isValuationCompatible = this.getWriteRegisters.subsetOf(conf.getRegisterNames)
    isStateValid //& isValuationCompatible
  }

  def getAllRegisters: Set[String] = getSelectedRegisters ++ getWriteRegisters

  def getSelectedRegisters: Set[String] = transitions.flatMap(t => {
    val sentenceRegisters = t.guard.sentence.getRegisterSelection
    sentenceRegisters
  }).toSet

  def getWriteRegisters: Set[String] = transitions.flatMap(_.writeRegisters).toSet

}
