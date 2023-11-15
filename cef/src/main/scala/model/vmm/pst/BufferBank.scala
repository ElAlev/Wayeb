package model.vmm.pst

import model.vmm.Symbol

/**
  * A buffer bank is just a collection of cyclic buffers. All buffers are of the same size. For each value of a
  * partition attribute, we maintain a cyclic buffer. If the buffer bank does not contain a buffer for a certain
  * partition value, a new buffer is created.
  *
  * @param maxOrder
  */
class BufferBank(maxOrder: Int) {
  private var bank: Map[String, CyclicBuffer] = Map.empty

  /**
    * Pushes a new symbol to the buffer of the given partition value.
    *
    * @param partitionId The partition value.
    * @param symbol The new symbol.
    */
  def push(
            partitionId: String,
            symbol: Symbol
          ): Unit = {
    if (bank.contains(partitionId)) bank(partitionId).pushSymbol(symbol)
    else {
      // if a buffer for the given partition value does not exist, create it
      val newBuffer = new CyclicBuffer(maxOrder)
      newBuffer.pushSymbol(symbol)
      bank += (partitionId -> newBuffer)
    }
  }

  /**
    * Pops the contents of the buffer for a given partition value.
    *
    * @param partitionId The partition value.
    * @return The buffer's contents, as a list of symbols, starting from the most recently added symbol.
    */
  def pop(partitionId: String): List[Symbol] = {
    require(bank.contains(partitionId))
    bank(partitionId).pop
  }

  /**
    * Clears the buffer for the given partition value.
    *
    * @param partitionId The given partition value.
    */
  def clear(partitionId: String): Unit = if (bank.contains(partitionId)) bank(partitionId).clear()


}
