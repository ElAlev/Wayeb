package fsm.symbolic.sra.dsra

import fsm.symbolic.sra.{SRA, SRATransition}

import java.io.{FileOutputStream, ObjectOutputStream}

object DSRA {
  def apply(
             states: Map[Int, DSRAState],
             transitions: List[SRATransition],
             start: Int,
             finals: Set[Int]
           ): DSRA = new DSRA(states, transitions, start, finals)
}

/**
 * Class representing deterministic symbolic register automata.
 *
 * @param states The automaton states, as a map of state ids to states.
 * @param transitions The list of transitions.
 * @param start The id of the start state.
 * @param finals The set of ids of the final states.
 */
case class DSRA private[dsra] (
                                states: Map[Int, DSRAState],
                                transitions: List[SRATransition],
                                override val start: Int,
                                override val finals: Set[Int]
                              ) extends SRA(states, transitions, start, finals) {

  def write2File(fn: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(this)
    oos.close()
  }

}
