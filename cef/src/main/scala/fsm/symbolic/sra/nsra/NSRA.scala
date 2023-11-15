package fsm.symbolic.sra.nsra

import fsm.symbolic.Valuation
import fsm.symbolic.sra.{SRA, SRATransition}
import stream.GenericEvent

import java.io.{FileOutputStream, ObjectOutputStream}

object NSRA {
  def apply(
             states: Map[Int, NSRAState],
             transitions: List[SRATransition],
             start: Int,
             finals: Set[Int]
           ): NSRA = new NSRA(states, transitions, start, finals)
}

/**
 * Class representing non-deterministic symbolic register automata.
 *
 * @param states The automaton states, as a map of state ids to states.
 * @param transitions The list of transitions.
 * @param start The id of the start state.
 * @param finals The set of ids of the final states.
 */
case class NSRA private[nsra] (
                                states: Map[Int, NSRAState],
                                transitions: List[SRATransition],
                                override val start: Int,
                                override val finals: Set[Int]
                              ) extends SRA(states, transitions, start, finals) {

  //override def getDeltaWithEpsilon(stateId: Int, event: GenericEvent): Set[Int] = Set.empty

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
