package stream.array

import model.markov.TransitionProbs
import stream.GenericEvent
import scala.collection.{SortedMap, mutable}
import scala.util.control.Breaks._

/**
  * Creates a random event stream from a set of conditional probabilities.
  *
  * @param size The size of the stream.
  * @param tp The set of conditional probabilities
  * @param s The seed for the random generator.
  */
class TransProbStream private[stream] (
                                        size: Int,
                                        tp: TransitionProbs,
                                        s: Int
                                      ) extends EventStreamI {
  require(size > 0 & size <= 10000000)
  private val order = tp.getOrder
  private val symbols = tp.getSymbols
  private val seed = s
  private var cumulativeProbs = mutable.Map[List[String], SortedMap[Double, String]]()
  require(checkProbs(tp))

  /**
    * Actually generates the stream.
    *
    * @return The event stream.
    */
  def generateStream(): EventStream = {
    val eventStream = new EventStream()
    val initSymbol = symbols.head
    val r = new scala.util.Random(seed)
    var p = 0.0
    for (i <- 1 to size) {
      if (i <= order) {
        eventStream.addEvent(GenericEvent(initSymbol, i))
      }
      else {
        p = r.nextDouble()
        breakable {
          val label = getLastEventsAsLabel(eventStream, i)
          val thisCumulativeProbs: SortedMap[Double, String] = cumulativeProbs(label)
          for ((k, v) <- thisCumulativeProbs) {
            if (p <= k) {
              eventStream.addEvent(GenericEvent(v, i))
              break
            }
          }
        }
      }
    }
    eventStream.setEventTypes(tp.getSymbols.toSet)
    eventStream.setProbs(tp)
    eventStream
  }

  private def getLastEventsAsLabel(
                                    es: EventStream,
                                    i: Int
                                  ): List[String] = {
    var label = List.empty[String]
    for (j <- i - order - 1 to i - 2) label = label ::: List(es.getEvent(j).eventType)
    label
  }

  private def checkProbs(p: TransitionProbs): Boolean = {
    val labels = createLabels(symbols, order)
    for (label <- labels) {
      var totalProb = 0.0
      var thisCumulativeProbs = SortedMap.empty[Double, String]
      for (symbol <- symbols) {
        val prob = p.getProb(label, symbol)
        if (prob < 0.0 | prob > 1.0) return false
        totalProb += prob
        if (totalProb > 1.0) return false
        thisCumulativeProbs += (totalProb -> symbol)
      }
      cumulativeProbs += (label -> thisCumulativeProbs)
    }
    true
  }

  private def createLabels(
                            symbols: mutable.Set[String],
                            k: Int
                          ): mutable.Set[List[String]] = {
    var labels = mutable.Set.empty[List[String]]
    labels += List.empty[String]
    var labelsK = mutable.Set.empty[List[String]]
    for (i <- 1 to k) {
      labelsK.clear()
      for (p <- labels) {
        for (e <- symbols) {
          val newp = p ::: List(e)
          labelsK += newp
        }
      }
      labels ++= labelsK
    }
    for (label <- labels) if (label.size != k) labels.remove(label)
    labels
  }

}
