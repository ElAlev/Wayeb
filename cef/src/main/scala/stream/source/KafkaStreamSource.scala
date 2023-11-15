package stream.source

import java.io.{File, FileReader}

import stream.array.EventStream
import java.util.Properties
import java.time.Duration

import com.typesafe.scalalogging.LazyLogging
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.consumer.ConsumerConfig
import stream.source.EmitMode.EmitMode

import scala.collection.JavaConverters._

object KafkaStreamSource {

  def apply(
             kafkaConf: String,
             domain: LineParser
           ): KafkaStreamSource = new KafkaStreamSource(kafkaConf, domain)
}


/**
  * Stream source that originates from Kafka.
  *
  * @param kafkaConf File path for kafka configuration file
  * @param domain The domain of the stream
  */
class KafkaStreamSource(
                         kafkaConf: String,
                         domain: LineParser
                       ) extends StreamSource with LazyLogging {

  /**
    * A consumer will be created with default properties taken from misc/KafkaConf.properties. It reads
    * messages from the kafka queue and forwards them to the listener.
    *
    * @param mode     The mode, BUFFER or ONLINE. Not used
    * @param timeout  The time (in seconds) the source is allowed to run. After the timeout, the source should stop
    *                 emitting events. Irrelevant here.
    * @return The stream as an array of events.
    */
  override def emitEvents(
                           mode: EmitMode,
                           timeout: Long
                         ): EventStream = {

    val consumer = setupConsumer
    var totalCounter = 1
    var streamActive = true
    while (streamActive) {
      val records = consumer.poll(Duration.ofMillis(1000))
      //if (records.isEmpty) streamActive = false
      for (record <- records.asScala) {
        if (record.value.equals("terminate")) streamActive = false
        else if (streamActive) {
          val event = domain.line2Event(record.value, totalCounter)
          totalCounter += 1
          send2Listeners(event)
        }
      }
    }
    consumer.close()
    new EventStream()

  }

  private def setupConsumer: KafkaConsumer[String, String] = {
    val props = new Properties()
    props.setProperty(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    props.setProperty(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    props.setProperty(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
    props.setProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")
    props.setProperty(ConsumerConfig.GROUP_ID_CONFIG, "defaultGroupIf")
    props.setProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
    val file = new File(kafkaConf)
    if (file.exists())
      props.load(new FileReader(file))

    val consumer = new KafkaConsumer[String, String](props)
    consumer.subscribe(java.util.Arrays.asList(props.getProperty("inputTopic", "wayebTopic")))
    consumer
  }

}
