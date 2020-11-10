package utils

/**
  * Utils for handling sets.
  */
object SetUtils {
  /**
    * Estimates the power set of a set.
    *
    * @param initialSet The initial set.
    * @tparam A The type of the elements of the initial set.
    * @return The power set of the initial set.
    */
  def power[A](initialSet: Set[A]): Set[Set[A]] = {
      @annotation.tailrec
      def pwr(t: Set[A], ps: Set[Set[A]]): Set[Set[A]] =
        if (t.isEmpty) ps
        else pwr(t.tail, ps ++ (ps map (_ + t.head)))
    pwr(initialSet, Set(Set.empty[A])) //Powerset of ∅ is {∅}
  }

  /**
    * Estimates all the permutations of given length from a given set of elements.
    *
    * @param elements The given set of elements.
    * @param length The length of the permutations.
    * @tparam T The type of the given elements.
    * @return All permutations of length length.
    */
  def permutations[T](
                       elements: Set[T],
                       length: Int
                     ): Set[List[T]] = {
    require(length > 0)
    if (length == 1) elements.map(e => List(e))
    else {
      val recEl = permutations(elements, length - 1)
      val expanded = for (re <- recEl; e <- elements) yield e :: re
      expanded
    }
  }

  /**
    * Estimates all the permutations up to a given length from a given set of elements.
    * Same as utils.SetUtils#permutations(scala.collection.immutable.Set, int), but is tail recursive.
    * Also returns not just permutation of the given length but all permutations of length 1, 2, etc up to length.
    *
    * @param elements The given set of elements.
    * @param length The length of the permutations.
    * @tparam T The type of the given elements.
    * @return All permutations up to the given length, as a map of lengths to sets of permutations.
    */
  def permutationsAlt[T](
                          elements: Set[T],
                          length: Int
                        ): Map[Int, Set[List[T]]] = {
    require(length > 0)
    val perms0 = Map[Int, Set[List[T]]]()
    permutationsAux1(elements, length, 1, perms0)
  }

  /**
    * Auxiliary recursive function to estimate all permutations up to the given length.
    *
    * @param elements The initial elements.
    * @param finalLength The final length of permutations where we stop.
    * @param currentLength The current length of permutations to be estimated.
    * @param previousPerms Current permutations of length currentLength-1.
    * @tparam T The type of elements.
    * @return All permutations up to the given length, as a map of lengths to sets of permutations.
    */
  @scala.annotation.tailrec
  def permutationsAux1[T](
                           elements: Set[T],
                           finalLength: Int,
                           currentLength: Int,
                           previousPerms: Map[Int, Set[List[T]]]
                         ): Map[Int, Set[List[T]]] = {
    require(currentLength > 0)
    currentLength match {
      case 1 => {
        val perms1 = Map(1 -> permutations(elements, 1))
        permutationsAux1(elements, finalLength, 2, perms1)
      }
      case l => {
        if (l == finalLength + 1) previousPerms
        else {
          val newPerms = permutationsAux2(elements, previousPerms(l - 1))
          val newPreviousPerms = previousPerms + (l -> newPerms)
          permutationsAux1(elements, finalLength, l + 1, newPreviousPerms)
        }
      }
    }
  }

  /**
    * Appends to a set of permutations all elements from a given set.
    *
    * @param elements The elements.
    * @param previousPerms The already existing permutations.
    * @tparam T The type of elements.
    * @return The new permutations.
    */
  private def permutationsAux2[T](
                                   elements: Set[T],
                                   previousPerms: Set[List[T]]
                                 ): Set[List[T]] = {
    val expanded = for (pp <- previousPerms; e <- elements) yield e :: pp
    expanded
  }

  /**
    * Estimates the cartesian product from given sets.
    *
    * @param sets The given sets.
    * @tparam T The type of elements of the sets.
    * @return The cartesian product.
    */
  def cartesian[T](sets: Set[Set[T]]): Set[Set[T]] = {
    require(sets.nonEmpty)
    require(sets.forall(s => s.nonEmpty))
    cartesianAux(Set.empty[Set[T]], sets.toList)
  }

  /**
    * Auxiliary recursive function to estimate the cartesian product.
    *
    * @param currentCartesian The current cartesian product.
    * @param remainingSets The sets the remain to be processed.
    * @tparam T The type of elements.
    * @return The cartesian product.
    */
  @scala.annotation.tailrec
  private def cartesianAux[T](
                               currentCartesian: Set[Set[T]],
                               remainingSets: List[Set[T]]
                             ): Set[Set[T]] = {
    remainingSets match {
      case Nil => currentCartesian
      case head :: tail => {
        val newCartesian =
          if (currentCartesian.isEmpty) head.map(h => Set(h))
          else { // currentCartesian.map(s => cartesian2(s,head)).map(ss => ss.flatten)
            var tmp = Set.empty[Set[T]]
            for (cc <- currentCartesian) {
              for (h <- head) {
                tmp = tmp + (cc + h)
              }
            }
            tmp
          }
        cartesianAux(newCartesian, tail)
      }
    }
  }

  /*private def cartesian2[T](
                             set1: Set[T],
                             set2: Set[T]
                           ): Set[Set[T]] = {
    require(set1.nonEmpty)
    if (set2.isEmpty) Set(set1)
    else {
      val cartesianTuples = cartesian2Aux(set1.toList, set2.toList, Set.empty[(T, T)])
      cartesianTuples.map(t => Set(t._1, t._2))
    }
  }

  @scala.annotation.tailrec
  private def cartesian2Aux[T](
                                list1: List[T],
                                list2: List[T],
                                out: Set[(T, T)]
                              ): Set[(T, T)] = {
    list1 match {
      case Nil => out
      case head :: tail => cartesian2Aux(tail, list2, out ++ cartesian2Aux(head, list2, Set.empty[(T, T)]))
    }
  }

  @scala.annotation.tailrec
  private def cartesian2Aux[T](
                                element: T,
                                list2: List[T],
                                out: Set[(T, T)]
                              ): Set[(T, T)] = {
    list2 match {
      case Nil => out
      case head :: tail => cartesian2Aux(element, tail, out ++ Set((element, head)))
    }
  }*/
}
