package model.vmm.pst

import com.typesafe.scalalogging.LazyLogging
import model.vmm.mapper.SymbolMapper
import stream.source.{EndOfStreamEvent, StreamListener}
import stream.{GenericEvent, ResetEvent}
import ui.ConfigUtils

object CSTLearner {

  /**
   * Constructor for CST Learner.
   *
   * @param maxOrder           The maximum order of the Prediction Suffix Tree to be constructed after the CST.
   * @param sm                 The symbol mapper which generates symbols for each new event.
   * @param partitionAttribute The SDFA's partition attribute.
   * @return                   The CST learner.
   */
  def apply(
             maxOrder: Int,
             sm: SymbolMapper,
             partitionAttribute: String
           ): CSTLearner = new CSTLearner(maxOrder, sm, partitionAttribute)
}

/**
  * Learner for Counter Suffix Trees. The learner is a stream listener, so we need to construct a stream source and send
  * to it the stream's events.
  *
  * @param maxOrder           The maximum order of the Prediction Suffix Tree to be constructed after the CST.
  * @param sm                 The symbol mapper which generates symbols for each new event.
  * @param partitionAttribute The SDFA's partition attribute.
  */
class CSTLearner(
                  maxOrder: Int,
                  sm: SymbolMapper,
                  partitionAttribute: String
                ) extends StreamListener with LazyLogging {
  // For each partition value we maintain a separate cyclic buffer.
  // Note that maxOrder is the maximum order of the PST. To accommodate this order, the order of the CST must be
  // maxOrder + 1. We need to be able to estimate conditional probabilities of a symbol given a context of length
  // maxOrder, so we need counter for words of length maxOrder + 1.
  private val bufferBank = new BufferBank(maxOrder + 1)
  private val cst = CounterSuffixTree()
  private var learningFinished = false

  /**
    * Processes every new event emitted from the training stream.
    *
    * @param event The new event.
    */
  override def newEventEmitted(event: GenericEvent): Unit = {
    event match {
      case _: EndOfStreamEvent => learningFinished = true // if we reached the end of the stream, set the relevant flag
      case _: ResetEvent => {
        val partitionId =
          if (partitionAttribute.equalsIgnoreCase(ConfigUtils.singlePartitionVal)) partitionAttribute
          else event.getValueOf(partitionAttribute).toString
        bufferBank.clear(partitionId)
      }
      case _ => {
        val sym = sm.evaluate(event)
        val partitionId =
          if (partitionAttribute.equalsIgnoreCase(ConfigUtils.singlePartitionVal)) partitionAttribute
          else event.getValueOf(partitionAttribute).toString
        bufferBank.push(partitionId, sym)
        val buffered = bufferBank.pop(partitionId)
        // note that even words of length smaller than maxSpread are used to build the CST
        cst.updateWithNewWord(buffered)
      }
    }
  }

  /**
    * @return The learnt CST. Emits a warning if learning has not finished (training stream has not reached its end).
    */
  def getCST: CounterSuffixTree = {
    if (learningFinished) cst
    else {
      logger.warn("Learning of CST not finished yet")
      cst
    }
  }
}
