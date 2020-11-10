package stream.array

import stream.GenericEvent
import scala.collection.mutable
import scala.util.control.Breaks._

/**
  * Creates a random event stream from a set of event type probabilities.
  *
  * @param size The size of the stream.
  * @param p The event type probabilities.
  * @param s The seed for the random generator.
  */
class ProbMapStream private[stream] (
                                      size: Int,
                                      p: mutable.Map[String, Double],
                                      s: Int
                                    ) extends EventStreamI {
  require(size > 0 & size <= 10000000)
  private var cumulativeProbs = mutable.Map.empty[String, Double]
  require(checkProbs(p))
  val probs: mutable.Map[String, Double] = p
  private val seed = s

  /**
    * @return The event stream.
    */
  def generateStream(): EventStream = {
    val eventStream = new EventStream()
    val r = new scala.util.Random(seed)
    var p = 0.0
    for (i <- 1 to size) {
      p = r.nextDouble()
      breakable {
        for ((k, v) <- cumulativeProbs) {
          if (p <= v) {
            eventStream.addEvent(GenericEvent(k, i))
            break
          }
        }
      }
    }
    eventStream.setEventTypes(probs.keys.toSet)
    eventStream.setProbs(probs)
    eventStream.setCumulativeProbs(cumulativeProbs)
    eventStream
  }

  private def checkProbs(p: mutable.Map[String, Double]): Boolean = {
    var totalProb = 0.0
    for ((k, v) <- p) {
      if (v < 0.0 | v > 1.0) return false
      totalProb += v
      if (totalProb > 1.0) return false
      cumulativeProbs += (k -> totalProb)
    }
    true
  }

}
