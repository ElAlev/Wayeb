package fsm.symbolic.sfa.logic

/**
  * Creates all permutations of the Boolean values TRUE/FALSE of length up to MaxLength.
  * fsm.symbolic.sfa.logic.BooleanPermutator#permutations() is a map. Each key is the length and each value a set with
  * all permutations of length equal to the key.
  *
  * NOTE: Used to be a singleton so that we can incrementally create permutations as needed. However, this can lead to
  * problems when running tests in parallel. Singleton shared by all threads.
  *
  * @param maxLength The maximum length of the permutations.
  */
class BooleanPermutator(var maxLength: Int) {
  private val truthValues = Set(true, false)
  private var permutations: Map[Int, Set[List[Boolean]]] = utils.SetUtils.permutationsAlt(truthValues, maxLength)

  /**
    * Retrieves all permutations of a certain given length.
    *
    * @param length The given length.
    * @return The permutations.
    */
  def getPermutations(length: Int): Set[List[Boolean]] = permutations(length)

  /**
    * Retrieves all permutations up to a certain given length.
    *
    * @param length The given length.
    * @return The permutations.
    */
  def getPermutationsUpTo(length: Int): Map[Int, Set[List[Boolean]]] = permutations.filter(p => p._1 <= length)

  /**
    * Expands permutations to a greater length, i.e., creates all extra permutations up to a new greater length.
    *
    * @param newLength the new length.
    */
  private def expand(newLength: Int): Unit = {
    require(newLength > maxLength)
    permutations = utils.SetUtils.permutationsAux1(truthValues, newLength, maxLength, permutations)
    maxLength = newLength
  }

}
