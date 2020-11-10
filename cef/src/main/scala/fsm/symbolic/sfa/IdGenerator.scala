package fsm.symbolic.sfa

import ui.ConfigUtils

object IdGenerator {
  final private val defaultMax: Int = ConfigUtils.idGeneratorMax

  def apply(alreadyGiven: Set[Int]): IdGenerator = new IdGenerator(defaultMax, alreadyGiven)

  def apply(
             max: Int,
             alreadyGiven: Set[Int]
           ): IdGenerator = new IdGenerator(max, alreadyGiven)

  def apply(max: Int): IdGenerator = new IdGenerator(max, Set.empty[Int])

  def apply(): IdGenerator = new IdGenerator(defaultMax, Set.empty[Int])
}

/**
  * An object of this class is used to generate unique IDs for automaton states.
  *
  * @param max The max state id.
  * @param alreadyGiven A set of IDs that have already been assigned to other states, if such states already exist.
  */
class IdGenerator(
                   max: Int,
                   alreadyGiven: Set[Int]
                 ) {
  require(max > 0)
  require(max > alreadyGiven.size)
  var idsGiven: Set[Int] = alreadyGiven

  /**
    * Creates a new ID, not already reserved by another state.
    *
    * @return The new ID.
    */
  def getId: Int = {
    if (idsGiven.size == max)
      throw new Error("Id generator cannot give new id (reached capacity: " + max + ")")
    val newId = if (idsGiven.isEmpty) 1
    else {
      val candidateId = (idsGiven.max + 1)
      if (idsGiven.contains(candidateId))
        throw new Error("Id generator cannot give new id (entered recycling)")
      else
        candidateId
    }
    idsGiven = idsGiven + newId
    newId
  }

  /**
    * Releases an ID in order to be usable again.
    *
    * @param id The ID to be released.
    */
  def releaseId(id: Int): Unit = {
    require(idsGiven.contains(id))
    idsGiven = idsGiven - id
  }

  def getGiven: Set[Int] = idsGiven

  def getGivenSize: Int = idsGiven.size

}
