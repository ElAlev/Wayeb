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
  //TODO: use mutable val
  private var idsGiven: Set[Int] = alreadyGiven
  private var maxGiven: Int = 0
  private var currentHead: Int = 0

  private val idsGivenMutable: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set[Int](alreadyGiven.toSeq:_*)

  def getIdCautiousMut: Int = {
    if (idsGivenMutable.size == max)
      throw new Error("Id generator cannot give new id (reached capacity: " + max + ")")
    val newId = {
      if (idsGivenMutable.isEmpty) {
        currentHead = 1
        1
      }
      else {
        var candidateId = if (currentHead == max) 1 else currentHead + 1
        while (idsGivenMutable.contains(candidateId)) {
          candidateId += 1
        }
        currentHead = candidateId
        candidateId
        /*if (idsGivenMutable.contains(candidateId))
          throw new Error("Id generator cannot give new id (entered recycling)")
        else {
          maxGiven = candidateId
          candidateId
        }*/
      }
    }
    idsGivenMutable += newId
    newId
  }

  /**
    * Creates a new ID, not already reserved by another state.
    *
    * @return The new ID.
    */
  def getIdCautiousImmut: Int = {
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

  def getIdGreedy: Int = {
    if (maxGiven == max) throw new Error("Id generator cannot give new id (reached capacity: " + max+1 + ")")
    val newId = maxGiven + 1
    maxGiven = newId
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

  def releaseIdMut(id: Int): Unit = {
    require(idsGivenMutable.contains(id))
    idsGivenMutable -= id
  }

  def releaseIdsMut(ids: Set[Int]): Unit = {
    ids.foreach(id => releaseIdMut(id))
  }

  def getGiven: Set[Int] = idsGiven

  def getGivenSize: Int = idsGiven.size

}
