package stream

import model.markov.TransitionProbs
import model.vmm.pst.psa.ProbSuffixAutomaton
import stream.array._
import stream.domain.homes.HomesLineParser
import stream.domain.stock.StockLineParser
import stream.domain.taxi.TaxiLineParser
import stream.source._
import ui.ConfigUtils
import utils.MathUtils.sampleUniform
//import stream.domain.archived.MaritimePairedDomain
import stream.domain.maritime.MaritimeLineParser

import scala.collection.mutable

/**
  * Factory for creating stream sources.
  */
object StreamFactory {
  /**
    * Creates a stream source from a file.
    * If the file is JSON, it will directly convert JSON attributes to event attributes.
    * If the file is CSV, we need to specify the domain so that the proper parser is called (for CSV files we need a
    * separate parser for each domain).
    *
    * @param fn The path to the file.
    * @param domain The domain:
    *               - json for JSON files, no separate parser required.
   *                - csv for CSV files.
    *               - maritime, for AIS messages in CSV.
    *               - ...
    * @param kafkaConf Configuration file for Kafka
    * @param args Any other arguments that the parser may need.
    * @return A stream source with the events of the file.
    */
  def getDomainStreamSource(
                             fn: String,
                             domain: String,
                             kafkaConf: String,
                             args: List[String]
                           ): StreamSource = {
    val isKafka: Boolean = if (fn.equalsIgnoreCase("kafka")) true else false
    domain match {
      case "json" => if (isKafka) KafkaStreamSource(kafkaConf, JsonLineParser) else JsonFileStreamSource(fn)
      case "maritime" => if (isKafka) KafkaStreamSource(kafkaConf, MaritimeLineParser) else CSVStreamSource(fn, MaritimeLineParser)
      case "csv" => CSVStreamSource(fn)
      case "stock" => CSVStreamSource(fn, StockLineParser)
      case "homes" => CSVStreamSource(fn, HomesLineParser)
      case "taxi" => CSVStreamSource(fn, TaxiLineParser)
      case _ => throw new IllegalArgumentException
    }
  }

  def getDomainStreamSource(
                             fn: String,
                             domain: String,
                             args: List[String]
                           ): StreamSource = getDomainStreamSource(fn, domain, ConfigUtils.defaultKafkaConf, args)

  def getDomainStreamSource(
                             args: List[String],
                             kafkaConf: String,
                             domain: String
                           ): StreamSource = getDomainStreamSource(fn = "kafka", domain, kafkaConf, args)

  /**
    * Creates an array source from a list of event types. Ids and timestamps given as increasing numbers.
    *
    * @param listOfEventTypes The list of event types.
    * @return An array source with the given list converted to events.
    */
  def getStreamSource(listOfEventTypes: List[String]): StreamSource = {
    val listStream = new ListStream(listOfEventTypes)
    ArrayStreamSource(listStream.generateStream())
  }

  def getStreamSource(
                       size: Int,
                       probs: mutable.Map[String, Double],
                       seed: Int,
                       values: Set[Int]
                     ): StreamSource = {
    val pmstream = new ProbMapStream(size: Int, probs: mutable.Map[String, Double], seed: Int)
    val initialStream = pmstream.generateStream().getStream
    val valuesList = values.toList
    val streamWithValues = initialStream.map(e => {
      val sampleIndex = sampleUniform(valuesList.size).toInt
      val value = valuesList(sampleIndex)
      GenericEvent(e.id, e.eventType, e.timestamp, Map("attr" -> value.toString))
    })
    ArrayStreamSource(EventStream(streamWithValues))
  }

  /**
    * Creates a random array source of a given size according to a set of given (conditional) probabilities. Event types
    * are those contained in the probabilities.
    *
    * @param size The size of the stream.
    * @param probs The probabilities from which to draw new events.
    * @param seed The seed for the random generator.
    * @return A random array source of the given size.
    */
  def getStreamSource(
                       size: Int,
                       probs: TransitionProbs,
                       seed: Int
                     ): StreamSource = {
    val tpstream = new TransProbStream(size: Int, probs: TransitionProbs, seed: Int)
    ArrayStreamSource(tpstream.generateStream())
  }

  /**
    * Creates a random array source of a given size according to a set of given event type probabilities. Event types
    * are those contained in the probabilities. No conditional probabilities given here. Evens are assumed to be i.i.d.
    * according to their given probabilities.
    *
    * @param size The size of the stream.
    * @param probs The probabilities from which to draw new events.
    * @param seed The seed for the random generator.
    * @return A random array source of the given size.
    */
  def getStreamSource(
                       size: Int,
                       probs: mutable.Map[String, Double],
                       seed: Int
                     ): StreamSource = {
    val pmstream = new ProbMapStream(size: Int, probs: mutable.Map[String, Double], seed: Int)
    ArrayStreamSource(pmstream.generateStream())
  }

  /**
    * Creates a random array source of a given size according to a set of given event type probabilities.
    * Stream size and event type probabilities are contained in a XML file. No conditional probabilities given here.
    * Evens are assumed to be i.i.d. according to their given probabilities.
    *
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
    * @param seed The seed for the random generator.
    * @return A random array source of the given size.
    */
  def getStreamSource(
                       fn: String,
                       seed: Int
                     ): StreamSource = {
    val xmlStream = new XMLStream(fn, seed)
    ArrayStreamSource(xmlStream.generateStream())
  }

  /**
    * Creates a generic stream source from a CSV file. First column assumed to be the event type and second the
    * timestamp.
    *
    * @param fn The path to the csv file.
    * @return A stream source with events from the file.
    */
  def getCSVStreamSource(fn: String): StreamSource = CSVStreamSource(fn)

  /**
    * Creates a random event stream (not a source) of a given size, using a probabilistic suffix automaton as generator.
    *
    * @param psa The probabilistic suffix automaton.
    * @param size The stream size.
    * @return The event stream along with a map from orders to percentage of labels generated by each order.
    */
  def getStream(
                 psa: ProbSuffixAutomaton,
                 size: Int
               ): (EventStream, scala.collection.immutable.Map[Int, Double]) = psa.generateStream(size)

}
