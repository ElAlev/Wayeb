package stream.array

import stream.GenericEvent
import scala.collection.mutable


/**
  * Converts a list of strings to an event stream. The list is assumed to contain the event types.
  *
  * @param l The list of strings.
  */
class ListStream private[stream] (l: List[String]) extends EventStreamI {
  private val inputList = l
  private var probs = mutable.Map.empty[String, Double]
  private var cumulativeProbs = mutable.Map.empty[String, Double]

  /**
    * Actually generates the stream.
    *
    * @return The event stream.
    */
  def generateStream(): EventStream = {
    val counter = mutable.Map.empty[String, Int]
    val eventStream = new EventStream()
    var i = 0
    for (e <- inputList) {
      i += 1
      eventStream.addEvent(GenericEvent(e, i))
      if (counter.contains(e)) {
        counter(e) += 1
      } else {
        counter += (e -> 1)
      }
    }
    calculateProbs(counter)
    eventStream.setEventTypes(probs.keys.toSet)
    eventStream.setProbs(probs)
    eventStream.setCumulativeProbs(cumulativeProbs)
    eventStream
  }

  private def calculateProbs(counter: mutable.Map[String, Int]): Unit = {
    val size = inputList.size.toDouble
    var cumProb = 0.0
    for ((k, v) <- counter) {
      val p = v.toDouble / size
      probs += (k -> p)
    }
    for ((k, v) <- probs) {
      cumProb += v
      cumulativeProbs += (k -> cumProb)
    }
  }

}
