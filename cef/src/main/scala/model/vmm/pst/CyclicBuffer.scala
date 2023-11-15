package model.vmm.pst

import model.vmm.Symbol

/**
  * Class for cyclic buffers. A cyclic buffer of size k is a buffer which, when full (has k symbols stored) and a new
  * symbol is pushed into it, the least recent (oldest) stored symbol is discarded to make room for the new symbol.
  *
  * @param size The buffer's size.
  */
class CyclicBuffer(val size: Int) {
  require(size > 0)
  private val buffer = new Array[Symbol](size)
  // Head is the pointer where the next symbol will be stored.
  // If head == -1, then the buffer is empty (no symbols stored).
  // Head >= 0 & Head <= size - 1.
  // If head == size - 1, then the buffer is full (all slots occupied).
  private var head = -1
  private val MaxHead = size - 1
  private var full = false

  /**
    * A new symbol is pushed into the buffer.
    *
    * @param symbol The new symbol.
    * @return The new head pointer.
    */
  def pushSymbol(symbol: Symbol): Int = {
    // first find the next head
    head = getNextHead
    // now store the new symbol (previous symbol, if exists, is overwritten)
    buffer(head) = symbol
    if (head == MaxHead) full = true
    head
  }

  /**
    * Pushes a word into the buffer.
    *
    * @param symbols The word as a list of symbols (the head of the list should be the last/most recent/rightmost symbol.
    * @return The new head.
    */
  def pushWord(symbols: List[Symbol]): Int = {
    val word = symbols.reverse // head of w is the most recent symbol, so we need to reverse it to start from the oldest
    require(word.size == size)
    pushAux(word, head)
  }

  /**
    * Auxiliary recursive function that pushes a word to the buffer.
    *
    * @param symbols The word as a list of symbols
    * @param tmpHead The temporary head as symbols are added.
    * @return The new head.
    */
  @scala.annotation.tailrec
  private def pushAux(
                       symbols: List[Symbol],
                       tmpHead: Int
                     ): Int = {
    symbols match {
      case head :: tail => pushAux(tail, pushSymbol(head))
      case Nil => tmpHead
    }
  }

  /**
    * Retrieves the contents of the buffer. Starts from head and goes back to retrieve all stored symbols.
    *
    * @return The buffer's contents as a list of symbols, where the head is the most recently added symbol.
    */
  def pop: List[Symbol] = {
    //require(head != -1)
    if (head == -1) List.empty[Symbol]
    // do not start popAux from head or you won't have a sound termination condition
    else popAux(getPreviousHead(head), List[Symbol](buffer(head)))
  }

  /**
    * Auxiliary recursive function that pops the contents of the buffer.
    *
    * @param currentHead The temporary head as symbols are popped.
    * @param output The temporary accumulator of symbols.
    * @return The popped symbols.
    */
  @scala.annotation.tailrec
  private def popAux(
                      currentHead: Int,
                      output: List[Symbol]
                    ): List[Symbol] = {
    if ((currentHead == -1) | (currentHead == head)) output.reverse
    else popAux(getPreviousHead(currentHead), buffer(currentHead) :: output)
  }

  /**
    * Finds the next head.
    *
    * @return The next head.
    */
  private def getNextHead: Int = {
    if (head < MaxHead) head + 1 // as long as head is smaller than MaxHead, we can simply increase by 1.
    else 0 // but if head == MaxHead, this means we have reached the last slot and need to go back at the start.
  }

  /**
    * Finds the previous head of a given head.
    *
    * @param currentHead The given head.
    * @return The previous head.
    */
  private def getPreviousHead(currentHead: Int): Int = {
    if (currentHead > 0) currentHead - 1 // simply decrease by 1, if given head is greater than 0
    else if (currentHead == 0) {
      // if given head is 0, we need to check whether the buffer is full
      if (full) MaxHead // if this is the case, then we need to wrap around and go to MaxHead
      else -1 // else there are no other symbols stored and we have reached the end
    } else -1
  }

  /**
    * Clears the buffer.
    * No need to actually delete anything. Just resets head and the full flag.
    */
  def clear(): Unit = {
    head = -1
    full = false
  }

  override def toString: String = buffer.toString + "\t head=" + head + "\t full=" + full
}
