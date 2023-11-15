package fsm.symbolic.sra.dsra

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sra.SRAUtils
import fsm.symbolic.sra.nsra.NSRAUtils
import fsm.symbolic.sre.SREFormula

object DSRAUtils extends LazyLogging {

  /**
   * Constructs a streaming DSRA from a formula f and a window.
   *
   * @param f       The given formula.
   * @param window  The window.
   * @return        The equivalent streaming DSRA.
   */
  def buildDSRAForStream(
                          f: SREFormula,
                          window: Int
                        ): DSRAStreaming = {
    // first, unroll
    logger.debug("Unrolling")
    val unrolledNSRA = NSRAUtils.buildUnrolledNSRA(f, window)
    // then determinize
    logger.debug("Determinizing")
    val dsra = SRAUtils.determinizeUnrolled(unrolledNSRA)
    // finally build the streaming version
    logger.debug("Streamifying")
    DSRAStreaming(dsra, window)
  }

  /*private def unrollVerticalClones(
                                    dsra: DSRA,
                                    window: Int
                                  ): List[DSRA] = {
    val laggedDSRAs = (1 until window).map(w => {
      addLagPrefix(dsra, w)
    }).toList

    val allDSRAs = dsra :: laggedDSRAs
    allDSRAs
  }

  @tailrec
  private def connectDSRAs(
                            dsra: DSRA,
                            remaining: List[DSRA]
                          ): DSRA = {
    remaining match {
      case Nil => dsra
      case head :: tail => {
        val newStates = dsra.states ++ head.states
        val newTransition = SRATransition(dsra.start, head.start, SRAGuard(SentenceConstructor.getNewTrueSentence), TransitionOutput.IGNORE)
        val newTransitions = (newTransition :: dsra.transitions) ::: head.transitions
        val newStart = dsra.start
        val newFinals = dsra.finals ++ head.finals
        val newDSRA = DSRA(newStates, newTransitions, newStart, newFinals)
        connectDSRAs(newDSRA, tail)
      }
    }
  }

  private def renameStatesRegisters(
                                     dsra: DSRA,
                                     startFrom: Int,
                                     regSuffix: String
                                   ): DSRA = {
    val oldRegisters = dsra.getAllRegisters.toList
    val old2newRegisters = oldRegisters.map(r => (r, r+regSuffix)).toMap
    val originalStateIds = dsra.states.keySet.toList
    val newStateIds = (startFrom until startFrom + originalStateIds.size).toList
    val orig2new = originalStateIds.zip(newStateIds).toMap
    //val new2orig = newStateIds.zip(originalStateIds).toMap
    val newStates: Map[Int, DSRAState] = dsra.states.map(s => {
      val newStateId = orig2new(s._1)
      val newState = DSRAState(newStateId)
      (newStateId, newState)
    })
    val newTransitions = dsra.transitions.map(t => {
      val newSource = orig2new(t.source)
      val newTarget = orig2new(t.target)
      val newGuard = SRAGuard(NSRAUtils.constructNewPhi(t.guard.sentence, old2newRegisters))
      val newWriteRegisters = t.writeRegisters.map(r => old2newRegisters(r))
      val newTransition = SRATransition(newSource, newTarget, newGuard, t.output, newWriteRegisters)
      newTransition
    })
    val newStart = orig2new(dsra.start)
    val newFinals = dsra.finals.map(f => orig2new(f))
    val newDsra = DSRA(newStates, newTransitions, newStart, newFinals)
    newDsra
  }

  private def addLagPrefix(
                            dsra: DSRA,
                            lagSteps: Int
                          ): DSRA = {
    require(lagSteps > -1)
    if (lagSteps <= 1) dsra
    else addLagStates(dsra, lagSteps)
  }

  @tailrec
  private def addLagStates(
                            dsra: DSRA,
                            lag: Int
                          ): DSRA = {
    require(lag >= 0)
    if (lag == 0) dsra
    else {
      val dsraWithOneExtraLag = addSingleLagState(dsra)
      addLagStates(dsraWithOneExtraLag, lag -1)
    }
  }

  private def addSingleLagState(dsra: DSRA): DSRA = {
    val newStartStateId = dsra.states.keySet.max + 1
    val newStartState = DSRAState(newStartStateId)
    val newGuard = SRAGuard(SentenceConstructor.getNewTrueSentence)
    val newTransition = SRATransition(newStartStateId, dsra.start, newGuard, TransitionOutput.IGNORE)
    val newTransitions = newTransition :: dsra.transitions
    val newStates: Map[Int, DSRAState] = dsra.states + (newStartStateId -> newStartState)
    val newDsra = DSRA(newStates, newTransitions, newStartStateId, dsra.finals)
    newDsra
  }

  private def resetFinalStates(dsra: DSRA): DSRA = {
    val finals = dsra.finals.toList
    resetStates(dsra, finals)
  }

  /**
   * Resets all given states into "start" states.
   *
   * @param dsra    The original DSRA.
   * @param toReset The states to reset.
   * @return        A DSRA with all given states reset.
   */
  @scala.annotation.tailrec
  private def resetStates(
                           dsra: DSRA,
                           toReset: List[Int]
                         ): DSRA = {
    toReset match {
      case Nil => dsra
      case head :: Nil => copyTransitions(dsra, dsra.start, head)
      case head :: tail => resetStates(copyTransitions(dsra, dsra.start, head), tail)
    }
  }

  /**
   * Copies all outgoing transitions of the "from" state as outgoing transitions of the "to" state and deletes all
   * other "to" transitions.
   *
   * @param dsra The original DSRA.
   * @param from The state from which to copy transitions.
   * @param to   The state to which we copy transitions.
   * @return     The DSRA with copied transitions.
   */
  private def copyTransitions(
                               dsra: DSRA,
                               from: Int,
                               to: Int
                             ): DSRA = {
    require(dsra.states.contains(from) & dsra.states.contains(to))
    val fromTransitions = dsra.transitions.filter(t => t.source == from)
    val newToTransitions = fromTransitions.map(t => SRATransition(to, t.target, t.guard.asInstanceOf[SRAGuard], t.output, t.writeRegisters))  // fromTransitions.map(t => SFATransition(to, t.target, t.guard.asInstanceOf[SFAGuard]))
    val intactTransitions = dsra.transitions.filter(t => t.source != to)
    val newTransitions = newToTransitions ::: intactTransitions
    DSRA(dsra.states, newTransitions, dsra.start, dsra.finals)
  }*/

}
