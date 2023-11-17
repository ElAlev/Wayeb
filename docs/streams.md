# Representing streams of input events

We currently use either files or Kafka topics to represent input streams.
See [stream](cef/src/main/scala/stream) package for all relevant code.

## Streams from files

Each event is a row and typically has a timestamp, an event type and a unique id.
Any other attributes may also be present as a Map of attribute names to values
(see [stream.GenericEvent](cef/src/main/scala/stream/GenericEvent.scala)).

The simplest possible way to represent a stream is to create a CSV file
with each line corresponding to an event.
The first column should contain the event type (as a String) and the second column the
event's timestamp (as a Long).
For this simple case, a parser is already available and you do not need to do anything else
(see [stream.source.GenericCSVLineParser](cef/src/main/scala/stream/source/GenericCSVLineParser.scala)).

If you need a stream of events with more attributes,
you can do so,
but you need to write a parser in order for the engine to know how to covert each line to and event.
For an example from the maritime domain,
see [stream.domain.maritime.MaritimeDomain](cef/src/main/scala/stream/domain/maritime/MaritimeDomain.scala).
If you do so,
then you need to also modify the function stream.StreamFactory.getDomainStreamSource
in [stream.StreamFactory](cef/src/main/scala/stream/StreamFactory.scala)
and add another option for your domain.
Then, whenever you run [recognition](docs/cep) or forecasting
(with [full-order](docs/ceffmm)  or [variable-order Markov models](docs/cefvmm)),
you need to specify the option *domainSpecificStream* (e.g., *--domainSpecificStream:maydomain*).

Alternatively, you may also represent a stream as a JSON file
(see [stream.source.JsonFileStreamSource](cef/src/main/scala/stream/source/JsonFileStreamSource.scala)).
Each JSON attribute will be converted to an event attribute.

## Reading input events from a Kafka topic

Besides reading events from a file,
Wayeb also has the ability to read events from a Kafka topic.
This Kafka topic could be written with events from a real-time stream,
thus enabling Wayeb to function in an online manner.

For example,
you may first compile a pattern:
```
java -jar wayeb-0.6.0-SNAPSHOT.jar compile --fsmModel:dsfa --patterns:patterns/maritime/port/pattern.sre --declarations:patterns/maritime/port/declarationsDistance1.sre --outputFsm:results/myFSM.fsm
```

Then, you can run recognition as follows:
```
java -jar wayeb-0.6.0-SNAPSHOT.jar recognition --fsmModel:dsfa --fsm:results/myFSM.fsm --stream:kafka --kafkaConf:kafkaConfigs/kafkaEarliest.properties --domainSpecificStream:maritime --statsFile:results/myFSM 
```

Compared to reading input events from a file,
there are two main differences when reading from a Kafka topic:
* The *stream* argument must be set to *kafka*;
* There is an extra argument, *kafkaConf*, pointing to a Kafka configuration file.
  In this file you must have declared the Kafka topic, the server and the port.
  An example config file exists in [kafkaConfigs/kafkaEarliest.properties](kafkaConfigs/kafkaEarliest.properties).
  It reads from the *wayebTopic*, from a local server at port 9092.
  This configuration makes Wayeb read from the very first record of the topic (auto.offset.reset=earliest)
  as opposed to the default *latest* that reads from the latest committed record.
  It will also keep start reading from this very first record whenever it restarts
  as no records are committed (enable.auto.commit=false).

Note that if Wayeb receives a *terminate* message,
it stops reading from the topic and produces statistics.

For the last command to work and for Wayeb to start detecting complex events,
there must obviously exist a Kafka topic,
as described in the configuration file.
Start Wayeb before the stream simulation if you don't want to miss any events.

## Stream simulator

You can populate the topic manually.
Alternatively, you may use the stream simulator which is provided with Wayeb.
For the simulator to work properly,
you must have Kafka installed on your machine.
Inside your kafka installation run the following commands to start zookeeper and the kafka servers:
```
./bin/zookeeper-server-start.sh config/zookeeper.properties &
./bin/kafka-server-start.sh config/server.properties
```

You may now start the simulator by running:
````
java -jar sim/target/scala-2.12/sim-0.6.0-SNAPSHOT.jar  csv --topic wayebTopic --streamfile:data/maritime/227592820.csv  --modifier:1000  --timepos 0 --idpos 1 --rate 200000 --delimeter ,
````
It will read events from a file and send them to wayebTopic.
It replays a stream 200000 times faster its actual speed.
Change the *rate* parameter from 200000 to 1 to replay the stream at its actual speed.
Change the *rate* to 0 to replay the stream as fast as possible.
*modifier* changes the timestamps from seconds to milliseconds as they appear as seconds in this dataset.


