package utils

import breeze.stats.distributions.Uniform
import com.typesafe.scalalogging.LazyLogging

object MiscUtils extends LazyLogging {

  /**
    * Randomly shuffles the elements of the list. Creates a new list with the same elements but with another random
    * order.
    *
    * @param initial The initial list.
    * @tparam T The type of elements.
    * @return A new list with the same elements randomly re-ordered.
    */
  def shuffleList[T](initial: List[T]): List[T] = shuffleListAux(List.empty[T], initial)

  /**
    * Auxiliary recursive function to randomly shuffle a list.
    *
    * @param shuffled The current shuffled list.
    * @param remaining The remaining elements from which to keep picking elements.
    * @tparam T The type of elements.
    * @return The shuffled list.
    */
  @scala.annotation.tailrec
  private def shuffleListAux[T](
                                 shuffled: List[T],
                                 remaining: List[T]
                               ): List[T] = {
    remaining match {
      case Nil => shuffled
      case _ :: _ => {
        val (sampledElement, remainingElements) = randomPick(remaining)
        shuffleListAux(sampledElement :: shuffled, remainingElements)
      }
    }
  }

  /**
    * Randomly picks an elements from a list. Returns the element and the rest of the list with the element removed.
    *
    * @param elements The initial list.
    * @tparam T The type of elements.
    * @return The randomly chosen element and the list with the element removed.
    */
  private def randomPick[T](elements: List[T]): (T, List[T]) = {
    val uni = Uniform(0, elements.length - 0.001)
    val sampleIndex = uni.sample().toInt
    val sampledElement = elements(sampleIndex)
    val remainingElements = (elements.toSet - sampledElement).toList
    (sampledElement, remainingElements)
  }
}
