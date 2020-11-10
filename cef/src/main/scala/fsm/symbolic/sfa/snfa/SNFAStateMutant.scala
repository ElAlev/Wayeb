package fsm.symbolic.sfa.snfa

import fsm.symbolic.sfa.Transition

object SNFAStateMutant {
  /**
    * Constructor for SNFA State mutant.
    *
    * @param id The unique id of the state.
    * @return The state.
    */
  def apply(id: Int): SNFAStateMutant = new SNFAStateMutant(id)
}

/**
  * Class representing the states of fsm.symbolic.sfa.snfa.SNFAMutantGraph, i.e., the states of a SNFA enooded as a
  * graph. Each such state should hold direct references to all of its next and previous states.
  *
  * @param id The unique id of the state.
  */
class SNFAStateMutant private[snfa] (val id: Int) {
  // The sets of next and previous states are mutable because each state is incrementally constructed by calls to
  // fsm.symbolic.sfa.snfa.SNFAStateMutant.addNext and fsm.symbolic.sfa.snfa.SNFAStateMutant.addPrevious.

  // the set of next states
  private val next = collection.mutable.Set[(SNFAStateMutant, Transition)]()
  // the set of previous states
  private val previous = collection.mutable.Set[(SNFAStateMutant, Transition)]()

  /**
    * Adds a next state.
    *
    * @param new_next The next state to be added with the relevant transition.
    */
  def addNext(new_next: (SNFAStateMutant, Transition)): Unit = next += new_next

  /**
    * Adds a previous state.
    *
    * @param new_previous The previous state to be added with the relevant transition.
    */
  def addPrevious(new_previous: (SNFAStateMutant, Transition)): Unit = previous += new_previous

  /**
    * @return All next states along with their transitions.
    */
  def getNext: collection.mutable.Set[(SNFAStateMutant, Transition)] = next

  /**
    * @return All previous states along with their transitions.
    */
  def getPrevious: collection.mutable.Set[(SNFAStateMutant, Transition)] = previous

}

