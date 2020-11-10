package stream.array

import stream.GenericEvent

import scala.collection.mutable
import scala.util.control.Breaks._

/**
  * Creates a random event stream according to event types and probabilities provided in a XML file.
  *
  * @param fn The path to the file.
  * @param seed The seed for the random generator.
  */
class XMLStream private[stream] (
                                  fn: String,
                                  seed: Int
                                ) extends EventStreamI {
  private var size = 0
  private var cumulativeProbs = mutable.Map.empty[String, Double]
  private var probs = mutable.Map.empty[String, Double]

  /**
    * Actually creates a stream.
    *
    * @return The event stream.
    */
  def generateStream(): EventStream = {
    val eventStream = new EventStream()
    parseStreamFile()
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

  /**
    * Parses the XML file.
    * Example xml file:
    * <stream>
    * <size>1000</size>
    * <event>
    * <type>a</type>
    * <probability>0.5</probability>
    * </event>
    * <event>
    * <type>b</type>
    * <probability>0.25</probability>
    * </event>
    * <event>
    * <type>c</type>
    * <probability>0.25</probability>
    * </event>
    * </stream>
    *
    */
  private def parseStreamFile(): Unit = {
    val fp = new XMLParser(fn)
    size = fp.getSize
    probs = fp.getProbs
    var cumProb = 0.0
    for ((k, v) <- probs) {
      cumProb += v
      cumulativeProbs += (k -> cumProb)
    }
  }
}
