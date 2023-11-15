package model.vmm

import utils.StringUtils

object SymbolWord {
  /**
    * Constructor for a word.
    *
    * @param word The list of symbols.
    * @return The word.
    */
  def apply(word: List[Symbol]): SymbolWord = new SymbolWord(word)

  /**
    * Constructor for an empty word.
    *
    * @return Empty word.
    */
  def apply(): SymbolWord = new SymbolWord(List.empty[Symbol])
}

/**
  * Class representing a word/string of symbols. A word is actually represented as a list of symbols. The head of the
  * list is the most recent/last/rightmost (assuming we read from left to right) symbol.
  *
  * @param word The list of symbols.
  */
class SymbolWord(val word: List[Symbol]) extends Serializable with Ordered[SymbolWord] {

  /**
    * Compares this word to another and returns their "difference". if the words are of different length, their
    * difference is the length difference. It they are the same length, their difference is the the difference of the
    * first symbol in which they differ, starting from the head/most recent symbol.
    *
    * @param other The other word.
    * @return Their difference.
    */
  override def compare(other: SymbolWord): Int = {
    if (this.length != other.length) this.length - other.length
    else compareAux(this, other)
  }

  /**
    * Auxiliary recursive function to estimate the difference of two words. Their difference is the the difference of
    * the first symbol in which they differ, starting from the head/most recent symbol.
    *
    * @param s1 The first word.
    * @param s2 The second word.
    * @return Their difference.
    */
  @scala.annotation.tailrec
  private def compareAux(
                          s1: SymbolWord,
                          s2: SymbolWord
                        ): Int = {
    require(s1.length == s2.length)
    if (s1.isEmpty) 0
    else if (s1.head != s2.head) s1.head.compare(s2.head)
    else compareAux(s1.tail, s2.tail)
  }

  /**
    * Determines whether this word is equal to another word. If the other object given is not a word, returns false.
    *
    * @param other The other word.
    * @return True if the other object is a word and their symbols list are equal.
    */
  override def equals(other: Any): Boolean = {
    other match {
      case other: SymbolWord => {
        other.canEqual(this) && checkLists(this.word, other.word)
      }
      case _ => false
    }
  }

  /**
    * Determines whether two list of symbols are equal. Must be the same length and have the same symbols in the same
    * positions.
    *
    * @param list1 The first list.
    * @param list2 The second list.
    * @return True if the two lists are equal.
    */
  @scala.annotation.tailrec
  private def checkLists(
                          list1: List[Symbol],
                          list2: List[Symbol]
                        ): Boolean = {
    if (list1.size != list2.size) false
    else {
      list1 match {
        case head :: tail => {
          if (head != list2.head) false
          else checkLists(tail, list2.tail)
        }
        case Nil => true
      }
    }
  }

  /**
    * Retrieves the suffix of the word. If the word has n symbols, the suffix is composed of the last n-1 symbols.
    *
    * @return The word's suffix.
    */
  def getSuffix: SymbolWord = {
    require(word.nonEmpty)
    SymbolWord(word.take(word.size - 1))
  }

  /**
    * Determines whether this word can be compared to another object. The other must also be a SymbolWord.
    *
    * @param other The other object.
    * @return True if the other is also an instance of SymbolWord.
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[SymbolWord]

  /**
    * @return Custom function for hash code.
    */
  override def hashCode(): Int = myHashCode(word)

  /**
    * Estimates a hash code for a list of symbols.
    *
    * @param l The list of symbols.
    * @return The hash code.
    */
  private def myHashCode(l: List[Symbol]): Int = {
    l match {
      case head :: tail => myHashCode(tail, 41 + head.hashCode())
      case Nil => l.hashCode() //throw new IllegalArgumentException("Cannot compute hash code for empty list.")
    }
  }

  /**
    * Auxiliary recursive function to estimate custom hash code for a list of symbols.
    *
    * @param l The list of symbols.
    * @param currentCode The hash code of the symbols processed thus far.
    * @return The list's hash code.
    */
  @scala.annotation.tailrec
  private def myHashCode(
                          l: List[Symbol],
                          currentCode: Int
                        ): Int = {
    l match {
      case head :: tail => myHashCode(tail, (41 * currentCode) + head.hashCode())
      case Nil => currentCode
    }
  }

  /**
    * @return The word's length.
    */
  def length: Int = word.size

  /**
    * @return True if the word is empty.
    */
  def isEmpty: Boolean = word.isEmpty

  /**
    * @return True if the word is not empty.
    */
  def nonEmpty: Boolean = word.nonEmpty

  /**
    * @return The word's last symbol, i.e., the list's head.
    */
  def head: Symbol = word.head

  /**
    * @return The word's first word.length-1 symbols, i.e., the list's tail.
    */
  def tail: SymbolWord = SymbolWord(word.tail)

  /**
    * Appends a new (last) symbol at the head of the list.
    *
    * @param symbol The new symbol.
    * @return The new word.
    */
  def appendHead(symbol: Symbol): SymbolWord = SymbolWord(symbol :: word)

  /**
    * Appends a symbol at the end of the list.
    *
    * @param symbol The new symbols to append.
    * @return The new word.
    */
  def appendTail(symbol: Symbol): SymbolWord = SymbolWord(word ::: List(symbol))

  /**
    * Determines if this word is a suffix of another given word. If this word has greater length, then false.
    *
    * @param other The other word.
    * @return True if this word is a suffix of the other.
    */
  def isSuffixOf(other: SymbolWord): Boolean = isSuffixOfAux(this, other)

  /**
    * Auxiliary recursive function to determine whether the first given word is a suffix of the second given word.
    *
    * @param word1 The first word.
    * @param word2 The second word.
    * @return True if the first is a suffix of the second.
    */
  @scala.annotation.tailrec
  private def isSuffixOfAux(
                             word1: SymbolWord,
                             word2: SymbolWord
                           ): Boolean = {
    if (word1.length > word2.length) false
    else {
      if (word1.isEmpty) true
      else {
        val head1 = word1.head
        val head2 = word2.head
        if (head1 != head2) false
        else isSuffixOfAux(word1.tail, word2.tail)
      }
    }
  }

  override def toString: String = StringUtils.list2Str(word)

}
