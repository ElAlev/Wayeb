package stream.array

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.xml._

/**
  * Parses for XML files used to generate random streams.
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
  * @param filename The path to the file.
  */
class XMLParser(filename: String) {
  private val probs = mutable.Map.empty[String, Double]
  private val loadnode: Elem = XML.loadFile(filename)
  private val size: Int = (loadnode \\ "size").text.toInt
  private val events: NodeSeq = loadnode \\ "event"
  var etype = ""
  var prob = 0.0
  for (event <- events) {
    etype = (event \\ "type").text
    prob = (event \\ "probability").text.toDouble
    probs += (etype -> prob)
  }
  var totalProb = 0.0
  for ((k, v) <- probs) {
    totalProb += v
  }
  if (totalProb != 1.0) {
    throw new IllegalArgumentException
  }

  def getSize: Int = size

  def getProbs: mutable.Map[String, Double] = probs

  def getEventTypes: Set[String] = probs.keys.toSet

}
