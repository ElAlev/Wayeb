import java.nio.file.{Files, Paths}
import java.util.Properties
import scala.io.Source
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import play.api.libs.json.Json

/**
  *
  * @param format The format of the data given in json or csv
  * @param filePath The file that contains the input stream to be simulated
  * @param delimeter The delimiter of the data in case of csv format
  * @param topic The output topic to write the simulated records
  * @param servers The kafka servers to connect to (e.g., localhost:9092 for local execution)
  * @param timePos The index of the timestamp column in case of csv format. Indexing starts from 0
  * @param idPos The index of the id column in case of csv format. Indexing starts from 0
  * @param rate The simulation rate at which each message is send. Actual time is divided by this number (e.g., 1 is real time. 10 is 10 times faster than real time). 0 sends records instantly
  * @param modifier adjusts the unit of measurement of the timestamp in the dataset to milliseconds. (e.g., if timestamps are in seconds give this value 1000)
  * @param timekey the timestamp key in case of json format
  * @param idkey the id key in case of json format
  */

case class SimulatorConfig(
                            format: String = "",
                            filePath: String = "",
                            delimeter: String = ",",
                            topic: String = "",
                            servers: String = "localhost:9092",
                            timePos: Int = 0,
                            idPos: Int = 1,
                            rate: Long = 0,
                            modifier: Double = 1,
                            timekey: String = "ts",
                            idkey: String = "id"
                          )


object StreamSimulator {


  def main(args: Array[String]): Unit = {

    val parser = new scopt.OptionParser[SimulatorConfig]("StreamSimulator") {

      help("help").text("prints this usage text")

      cmd("csv").
        action((_, c) => c.copy(format = "csv")).
        text("The format of the data in csv").
        children(
          opt[String]("streamfile").required().valueName("<file path>").
            action((x, c) => c.copy(filePath = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Stream file does not exist")).
            text("The file that simulates the stream."),
          opt[String]("delimeter").valueName("<file path>").
            action((x, c) => c.copy(delimeter = x)).
            text("The delimeter string to split the csv data. Default is \",\""),
          opt[String]("topic").required().valueName("<file path>").
            action((x, c) => c.copy(topic = x)).
            text("The output topic to write the stream"),
          opt[String]("servers").valueName("<file path>").
            action((x, c) => c.copy(servers = x)).
            text("the kafka servers to connect to. Default is \"localhost:9092\""),
          opt[Long]("rate").required().valueName("Long >=0").
            action((x, c) => c.copy(rate = x)).
            validate(x =>
              if (x >= 0) success
              else failure("Rate must be greater or equal to zero")).
            text("The rate at which messages are sent to the topic. Actual time (taken from timestamps) will be divided by this number"),
          opt[Double]("modifier").valueName("Double >0").
            action((x, c) => c.copy(modifier = x)).
            validate(x =>
              if (x > 0) success
              else failure("Modifier must be greater than zero")).
            text("Adjusts the time to milliseconds. E.g., If the timestamps are in seconds give this value 1000"),
          opt[Int]("timepos").required().valueName("Int >=0").
            action((x, c) => c.copy(timePos = x)).
            text("the position of the timestamp column"),
          opt[Int]("idpos").required().valueName("Int >=0").
            action((x, c) => c.copy(idPos = x)).
            text("the position of the id column")
        )

      cmd("json").
        action((_, c) => c.copy(format = "json")).
        text("The format of the data in json").
        children(
          opt[String]("streamfile").required().valueName("<file path>").
            action((x, c) => c.copy(filePath = x)).
            validate(x =>
              if (Files.exists(Paths.get(x))) success
              else failure("Stream file does not exist")).
            text("The file that simulates the stream."),
          opt[String]("topic").required().valueName("<file path>").
            action((x, c) => c.copy(topic = x)).
            text("The output topic to write the stream"),
          opt[String]("servers").valueName("<file path>").
            action((x, c) => c.copy(servers = x)).
            text("the kafka servers to connect to"),
          opt[Long]("rate").valueName("Long >=0").
            action((x, c) => c.copy(rate = x)).
            validate(x =>
              if (x >= 0) success
              else failure("Rate must be greater or equal to zero")).
            text("The rate at which messages are sent to the topic. Actual time (taken from timestamps) will be divided by this number"),
          opt[Double]("modifier").valueName("Double >0").
            action((x, c) => c.copy(modifier = x)).
            validate(x =>
              if (x > 0) success
              else failure("Modifier must be greater than zero")).
            text("Adjusts the time to milliseconds. E.g., If the timestamps are in seconds give this value 1000"),
          opt[String]("timekey").valueName("<file path>").
            action((x, c) => c.copy(timekey = x)).
            text("the json key to get the timestamp"),
          opt[String]("idkey").valueName("<file path>").
            action((x, c) => c.copy(idkey = x)).
            text("the json key to get the id"),
        )
    }

    parser.parse(args, SimulatorConfig()) match {
      case Some(config) => {

        val format = config.format
        val filePath = config.filePath
        val delimeter = config.delimeter
        val timePos = config.timePos
        val idPos = config.idPos
        val maxPos = if (timePos > idPos) timePos else idPos
        val topic = config.topic
        val rate = config.rate
        val modifier = config.modifier
        val servers = config.servers
        val idkey = config.idkey
        val timekey = config.timekey

        val props = new Properties()
        props.put("bootstrap.servers", servers)
        props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
        props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
        val producer = new KafkaProducer[String, String](props)
        val file = Source.fromFile(filePath)
        var prevTs: Long = 0
        var mod: Long = 0
        var negWaitTime: Long = 0
        var clock: Long = 0
        /**
          * The formula that simulates the rate at which events are sent. Not used when max speed is selected
          * Out of order events are sent instantly as events with larger timestamp have already been processed (i.e., curTs <= prevTs)
          * Small deviation at targeted simulated time since command *if (waitTime > 0)* is not calculated as part of the loop processing time
          * The variables below are needed for the next iteration/message and hence can not be defined as arguments of simulateRate (i.e., their values are changed)
          * prevTs: the timestamp of the previous event
          * mod: Timestamps are integers and we need the remainder of ((curTs - prevTs) / rate) to be taken into account in the next iteration
          * negWaitTime: If the waitTime is negative it is removed in the next iteration to catch up
          * clock: clock - System.currentTimeMillis() gives us the loop processing time. Also needs to be subtracted from the waitTime
          * @param curTs The timestamp of the current event
          * @param id The id of the current event
          * @param msg The event message
          */
        def simulateRate(curTs: Long, id: String, msg: String): Unit = {
          if (curTs > prevTs) {
            if (prevTs == 0) prevTs = curTs
            val diff = curTs - prevTs + mod
            val waitTime = diff / rate + negWaitTime + clock - System.currentTimeMillis()
            if (waitTime > 0) Thread.sleep(waitTime)
            clock = System.currentTimeMillis()
            negWaitTime = if (waitTime < 0) waitTime else 0
            mod = diff % rate
            prevTs = curTs
          }
          val record = new ProducerRecord[String, String] (topic, id, msg)
          producer.send (record)
        }

        clock = System.currentTimeMillis()
        format match {
          // SEND INSTANTLY CSV
          case "csv" => if (rate == 0) {
            for (msg <- file.getLines () ) {
              val columns = msg.split (delimeter, idPos + 2)
              val id = columns (idPos)
              val record = new ProducerRecord[String, String] (topic, id, msg)
              producer.send (record)
            }
          }
          // ELSE SIMULATE SPECIFIED RATE
          else
            for (msg <- file.getLines () ) {
              val columns = msg.split (delimeter, maxPos + 2)
              val curTs = (columns(timePos).toDouble * modifier).toLong
              val id = columns (idPos)
              simulateRate(curTs, id, msg)
          }

          // SEND INSTANTLY JSON
          case "json" => if (rate == 0) {
            for (msg <- file.getLines () ) {
              val map = Json.parse(msg)
              val id = map(idkey).toString
              val record = new ProducerRecord[String, String] (topic, id, msg)
              producer.send (record)
            }
          }
          // ELSE SIMULATE SPECIFIED RATE
          else
            for (msg <- file.getLines () ) {
              val map = Json.parse(msg)
              val curTs = (map(timekey).as[Double]* modifier).toLong
              val id = map(idkey).toString
              simulateRate(curTs, id, msg)
            }

          case x => {file.close(); producer.close(); throw new IllegalArgumentException(x)}

        }
        file.close()
        val numOfPartitions = producer.partitionsFor(topic).size
        for (partition <- 0 until numOfPartitions)
          producer.send(new ProducerRecord[String, String] (topic, partition, null, "terminate"))
        producer.close()
      }

      case None => throw new IllegalArgumentException("Something is wrong with the arguments.")
    }
  }



}
