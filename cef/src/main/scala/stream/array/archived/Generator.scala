package stream.array.archived

import stream.GenericEvent
import stream.array.{EventStream, XMLParser}

import scala.collection.mutable.Map
import scala.util.control.Breaks._

class Generator {
  private var filename = ""
  private val eventStream = new EventStream()
  private var cumulativeProbs = Map.empty[String, Double]
  private var probs = Map.empty[String, Double]
  private var size = 0
  private var seed = 100

  def setFilename(fn: String): Unit = { filename = fn }

  def setSeed(s: Int): Unit = { seed = s }

  def parseStreamFile(): Unit = {
    val fp = new XMLParser(filename)
    size = fp.getSize
    var cumProb = 0.0
    for ((k, v) <- fp.getProbs) {
      cumProb += v
      cumulativeProbs += (k -> cumProb)
    }
    probs = fp.getProbs
  }

  def generateStream(): EventStream = {
    eventStream.clearStream()
    val r = new scala.util.Random(seed)
    var p = 0
    for (i <- 1 to size) {
      p = r.nextInt(100) + 1
      breakable {
        for ((k, v) <- cumulativeProbs) {
          if (p <= v) {
            eventStream.addEvent(GenericEvent(k.toString, i))
            break
          }
        }
      }
    }
    eventStream
  }

  def generateStream(el: List[Char]): EventStream = {
    eventStream.clearStream()
    var i = 0
    for (e <- el) {
      i += 1
      eventStream.addEvent(GenericEvent(e.toString, i))
    }
    eventStream
  }

  def getStream: EventStream = eventStream

  def getCumulativeProbs: Map[String, Double] = cumulativeProbs

  def getProbs: Map[String, Double] = probs
}
