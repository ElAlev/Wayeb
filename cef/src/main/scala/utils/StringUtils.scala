package utils

/**
  * Some utils for handling strings.
  */
object StringUtils {

  /**
    * Converts a list to a string where each element is separated by , .
    *
    * @param args The list to be stringified.
    * @tparam T The type of list elements.
    * @return The stringified list.
    */
  def list2Str[T](args: List[T]): String = list2Str(args, "", ",")

  /**
    * Converts a list to a string where each element is separated by the given separator.
    *
    * @param args The list to be stringified.
    * @param separator The separator.
    * @tparam T The type of list elements.
    * @return The stringified list.
    */
  def list2Str[T](
                   args: List[T],
                   separator: String
                 ): String = list2Str(args, "", separator)

  /**
    * Auxiliary recursive function for
    * utils.StringUtils#list2Str(scala.collection.immutable.List, java.lang.String, java.lang.String).
    *
    * @param args The list to be stringified.
    * @param currentString The string accumulator.
    * @param separator The separator.
    * @tparam T The type of list elements.
    * @return The stringified list.
    */
  @scala.annotation.tailrec
  def list2Str[T](
                   args: List[T],
                   currentString: String,
                   separator: String
                 ): String = {
    args match {
      case Nil => currentString
      case last :: Nil => currentString + last.toString
      case head :: tail => list2Str(tail, currentString + head.toString + separator, separator)
    }
  }
}
