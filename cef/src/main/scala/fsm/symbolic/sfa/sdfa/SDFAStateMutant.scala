package fsm.symbolic.sfa.sdfa

import fsm.symbolic.sfa.Transition
import fsm.symbolic.sfa.logic.Sentence

object SDFAStateMutant {
  /**
    * Constructor for SDFAStateMutant.
    *
    * @param id The unique id of the state.
    * @return The SDFAStateMutant.
    */
  def apply(id: Int): SDFAStateMutant = new SDFAStateMutant(id)
}

/**
  * Class representing the states of fsm.symbolic.sfa.sdfa.SDFAMutantGraph, i.e., the states of a SDFA enooded as a
  * graph. Each such state should hold direct references to all of its next and previous states.
  *
  * @param id The unique id of the state.
  */
class SDFAStateMutant private[sdfa] (val id: Int) {
  // The sets of next and previous states are mutable because each state is incrementally constructed by calls to
  // fsm.symbolic.sfa.sdfa.SDFAStateMutant.addNext and fsm.symbolic.sfa.sdfa.SDFAStateMutant.addPrevious.

  // the set of next states
  private val next = collection.mutable.Set[(SDFAStateMutant, Transition)]()
  // the set of previous states
  private val previous = collection.mutable.Set[(SDFAStateMutant, Transition)]()

  def addNext(new_next: (SDFAStateMutant, Transition)): Unit = next += new_next

  def addPrevious(new_previous: (SDFAStateMutant, Transition)): Unit = previous += new_previous

  def removeNext(
                  str: SDFAStateMutant,
                  ser: Sentence
                ): Unit = {
    val state = next.filter(el => (el._1 == str & el._2.guard.sentence == ser))
    next --= state
  }

  def removePrevious(
                      str: SDFAStateMutant,
                      ser: Sentence
                    ): Unit = {
    val state = previous.filter(el => (el._1 == str & el._2.guard.sentence == ser))
    previous --= state
  }

  def getNext(s: Sentence): collection.mutable.Set[SDFAStateMutant] =
    next.filter(el => el._2.guard.sentence == s).map(el1 => el1._1)

  def getNext: collection.mutable.Set[(SDFAStateMutant, Transition)] = next

  def getPrevious: collection.mutable.Set[(SDFAStateMutant, Transition)] = previous

/**************************************/
  /** Methods below not currently used **/
/**************************************/

  @deprecated
  def addNext(new_next: collection.mutable.Set[(SDFAStateMutant, Transition)]): Unit = next ++= new_next

  @deprecated
  def addPrevious(new_previous: collection.mutable.Set[(SDFAStateMutant, Transition)]): Unit = previous ++= new_previous

  @deprecated
  def removeNext(r: Sentence): Unit = {
    val state = next.filter(el => el._2.guard.sentence == r)
    next --= state
  }

  @deprecated
  def removeNext(r: SDFAStateMutant): Unit = {
    val state = next.filter(el => el._1 == r)
    next --= state
  }

  @deprecated
  def removeNext(r: Int): Unit = {
    val state = next.filter(el => el._1.id == r)
    require(state.size == 1)
    next -= state.head
  }

  @deprecated
  def removePrevious(r: Sentence): Unit = {
    val state = previous.filter(el => el._2.guard.sentence == r)
    previous --= state
  }

  @deprecated
  def removePrevious(r: SDFAStateMutant): Unit = {
    val state = previous.filter(el => el._1 == r)
    previous --= state
  }

  @deprecated
  def removePrevious(r: Int): Unit = {
    val state = previous.filter(el => el._1.id == r)
    require(state.size == 1)
    previous -= state.head
  }

}
