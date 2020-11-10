package stream

import model.markov.TransitionProbs
import stream.array.{ListStream, ProbMapStream, TransProbStream, XMLStream}
import stream.domain.bio.BioLineParser
import stream.source.{ArrayStreamSource, CSVStreamSource, JsonLineParser, JsonFileStreamSource, KafkaStreamSource, StreamSource}
import ui.ConfigUtils
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
    *               - cards, for credit cards transactions in CSV.
    *               - maritime, for AIS messages in CSV.
    *               - vodafone.
    *               - caviar.
    *               - text.
    *               - bio.
    * @param isKafka Whether the source originates from Kafka or a file
    * @param kafkaConf Configuration file for Kafka
    * @param args Any other arguments that the parser may need.
    * @return A stream source with the events of the file.
    */
  def getDomainStreamSource(
                             fn: String,
                             domain: String,
                             isKafka: Boolean,
                             kafkaConf: String,
                             args: List[String]
                           ): StreamSource = {
    domain match {
      case "json" => if (isKafka) KafkaStreamSource(kafkaConf, JsonLineParser) else JsonFileStreamSource(fn)
      case "maritime" => if (isKafka) KafkaStreamSource(kafkaConf, MaritimeLineParser) else CSVStreamSource(fn, MaritimeLineParser)
      case "bio" => if (isKafka) KafkaStreamSource(kafkaConf, BioLineParser) else CSVStreamSource(fn, BioLineParser)
      case _ => throw new IllegalArgumentException
    }
  }

  def getDomainStreamSource(
                             fn: String,
                             domain: String,
                             args: List[String]
                           ): StreamSource = getDomainStreamSource(fn, domain, isKafka = false, ConfigUtils.defaultKafkaConf, args)

  def getDomainStreamSource(
                             args: List[String],
                             kafkaConf: String,
                             domain: String
                           ): StreamSource = getDomainStreamSource(fn = "", domain, isKafka = true, kafkaConf, args)

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

}
