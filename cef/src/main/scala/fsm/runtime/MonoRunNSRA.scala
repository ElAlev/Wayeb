package fsm.runtime

import com.typesafe.scalalogging.LazyLogging
import fsm.WindowType.COUNT
import fsm.{FSMInterface, NSRAInterface}
import fsm.symbolic.TransitionOutput.{TAKE, TransitionOutput}
import fsm.symbolic.sra.Configuration
import stream.GenericEvent
import scala.annotation.tailrec
import scala.collection.mutable

object MonoRunNSRA {

  /**
   * Constructor for MonoRunNSRA.
   *
   * @param fsm         The mono-run's FSM.
   * @param show        Determines whether complex event matches are to be displayed or not. Should be FALSE when
   *                    running experiments. Slows the engine down.
   * @param postProcess Determines whether complex event matches will get a minimal processing after being detected.
   *                    Useful to ensure that the simple evens comprising a complex one do get accessed and not just
   *                    ignored.
   * @return A MonoRunNSRA.
   */
  def apply(
             fsm: FSMInterface,
             show: Boolean,
             postProcess: Boolean
           ): MonoRunNSRA = new MonoRunNSRA(fsm, show, postProcess)
}

/**
 * Class representing a NSRA mono-run.
 *
 * @param fsm           The mono-run's FSM.
 * @param show          Determines whether complex event matches are to be displayed or not. Should be FALSE when
 *                      running experiments. Slows the engine down.
 * @param postProcess   Determines whether complex event matches will get a minimal processing after being detected.
 *                      Useful to ensure that the simple evens comprising a complex one do get accessed and not just
 *                      ignored.
 */
class MonoRunNSRA(
                   val fsm: FSMInterface,
                   val show: Boolean,
                   postProcess: Boolean
                 ) extends LazyLogging {
  // Gather final states in a local variable for faster access.
  private val localFinals: mutable.HashSet[Int] = mutable.HashSet.empty
  fsm.getFinals.foreach(f => localFinals += f)
  // Local variable for window for faster access.
  private val runtimeWindow = fsm.getRuntimeWindow

  private var dummyForPostProcessing: Int = 0


  /**
   * Makes a move on a given run according to its active state.
   * Also accumulates updated runs.
   *
   * @param event                 The new event.
   * @param currentConf           The current configuration of the run.
   * @param eventCounter          The event index.
   * @param matchedEvents         The current match of the run.
   * @param previousStatesMatches The accumulator holding previous runs.
   * @return The number of full matches / complex events and the accumulated runs.
   */
  def moveRunArrayPrealloc(
                            event: GenericEvent,
                            currentConf: Configuration,
                            eventCounter: Long,
                            matchedEvents: MatchList,
                            previousStatesMatches: List[(Configuration, MatchList)]
                          ): (Int, List[(Configuration, MatchList)]) = {
    var newStatesMatches = previousStatesMatches
    var detected = 0
    val r = if (checkRuntimeWindow(eventCounter, event.timestamp, matchedEvents.getMinCounter)) {
      fsm match {
        case x: NSRAInterface => {
          // First estimate next configuration.
          val nextConfs = x.nsra.yieldsSuccessorConfigNoEpsilonOpt(currentConf, event)
          var i = 0
          var l = List.empty[(Configuration, MatchList)]
          // For each new configuration
          while (i < nextConfs.length) {
            val nextConf = nextConfs(i)
            if (nextConf != null) {
              val newMatch = postProcessMatchList(event, nextConf.stateId, nextConf.output, eventCounter, matchedEvents.clone())
              // check if we have a new complex event
              if (newMatch._1) {
                detected += 1
              }
              else { // otherwise "create" a new run and add it to the accumulator
                l = (nextConf, newMatch._2) :: l
                newStatesMatches = (nextConf, newMatch._2) :: newStatesMatches
              }
            }
            i += 1
          }
          newStatesMatches
        }
        case _ => throw new Exception("Only NSRAs can use this method.")
      }
    }
    else {
      previousStatesMatches
    }
    (detected, r)
  }

  /**
   * Checks if we need to add the event to the match.
   * Also checks if we have a complex event.
   * May also perform a little bit of dummy post-processing to ensure that simple events are accessed after a detection.
   *
   * @param event         The new event.
   * @param stateId       The next state.
   * @param output        The next output.
   * @param eventCounter  The event index.
   * @param matchedEvents The current (partial) match.
   * @return True if complex event was detected. Plus, the new match.
   */
  private def postProcessMatchList(
                                    event: GenericEvent,
                                    stateId: Int,
                                    output: TransitionOutput,
                                    eventCounter: Long,
                                    matchedEvents: MatchList
                                  ): (Boolean, MatchList) = {
    val nextState = stateId
    val detected = localFinals.contains(nextState)
    if (output == TAKE) matchedEvents.addEvent(event, eventCounter, fsm.getWindowType)
    if (detected) {
      matchedEvents.setFull(true)
      if (show) {
        val msg = "\nMATCH: " +
          //"Attr->" + attributeValue +
          " Timestamp->" + event.timestamp.toLong +
          //" State->" + nextState +
          " Events->" + matchedEvents.toString() +
          "\n"
        logger.info(msg)
      }
      else if (postProcess) {
        postProcessMatch(matchedEvents)
      }
      matchedEvents.clear()
    }
    (detected, matchedEvents)
  }

  /**
   * A little bit of dummy post-processing of a match.
   *
   * @param m The match.
   */
  private def postProcessMatch(m: MatchList): Unit = postProcessMatchAux(m.getEvents)

  /**
   * A little bit of dummy post-processing of a match. Auxiliary recursive function.
   *
   * @param m The match.
   */
  @tailrec
  private def postProcessMatchAux(m: List[Int]): Unit = {
    m match {
      case Nil => {}
      case head :: tail => {
        if (head % 10 == 0) dummyForPostProcessing += 1
        postProcessMatchAux(tail)
      }
    }
  }

  /**
   * Checks if the window constraint is violated with the new event.
   * Two types of windows:
   *  - count based: in this case, we use the event index.
   *  - time based: in this case, we use the event timestamp.
   *
   * @param eventCounter   The new event index.
   * @param eventTimestamp The new event timestamp.
   * @param minCounter     The index/timestamp of the first event in the match.
   * @return True if the window constraint is ok (i.e., not violated).
   */
  private def checkRuntimeWindow(
                                  eventCounter: Long,
                                  eventTimestamp: Long,
                                  minCounter: Long
                                ): Boolean = {
    // if window = 0, this means that there is no window
    if (runtimeWindow == 0) true
    else {
      // if there are no events in the match (minCounter = -1), the window constraint cannot be violated
      if (minCounter == -1) true
      else {
        // the window constraint is violated if the window value is smaller than the interval spanned by the matched
        // events
        val diff = if (fsm.getWindowType == COUNT) Math.abs(eventCounter - minCounter) else Math.abs(eventTimestamp - minCounter)
        (diff < runtimeWindow)
      }
    }
  }

  def reset(): Unit = {

  }

}
