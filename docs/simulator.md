# Running example for simulating a stream

Clone Wayeb. Let's assume Wayeb's Home is $WAYEB_HOME.

```
cd $WAYEB_HOME
sbt assembly
```

This will create the fat jar *$WAYEB_HOME/cef/target/scala-2.12/wayeb-0.2.0-SNAPSHOT.jar*. 
You can use this jar to run Wayeb. It will also create the fat jar *$WAYEB_HOME/sim/target/scala-2.12/sim-0.2.0-SNAPSHOT.jar*. You can use this jar to run the simulator.

Have Kafka installed on your machine. Inside your kafka installation run the following commands to make zookeeper and kafka servers running:

```
./bin/zookeeper-server-start.sh config/zookeeper.properties &
./bin/kafka-server-start.sh config/server.properties
```

Type --help to print all the arguments
```
java -jar sim/target/scala-2.12/sim-0.2.0-SNAPSHOT.jar --help
```

The following command replays a stream 200000 times faster its actual speed. Change the *rate* paramater from 200000 to 1 to replay the stream at its actual speed. Change the *rate* to 0 to replay the stream as fast as possible. *modifier* changes the timestamps from seconds to milliseconds as they appear as seconds in this dataset

```
java -jar ${WAYEB_HOME}/sim/target/scala-2.12/sim-0.2.0-SNAPSHOT.jar  csv --topic defaultTopic --streamfile data/maritime/enriched/1443650401-1459461585_gap1800_interval60_speed1.0_matches360/port/all/allvessels.csv --modifier:1000  --timepos 0 --idpos 1 --rate 200000 --delimeter ,
```

Test the simulated stream using a kafka console consumer. Inside your kafka installation run:

```
bin/kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic defaultTopic
```

#Wayeb reading from Kafka

Alternatively, you can have Wayeb consuming from the topic of the simulated stream. First compile some patterns and then start the recognition.

```
java -jar cef/target/scala-2.12/wayeb-0.2.0-SNAPSHOT.jar compile --patterns:patterns/maritime/port/pattern.sre --declarations:patterns/maritime/port/declarationsDistance1.sre --outputFsm:results/myFSM.fsm
```
```
java -jar cef/target/scala-2.12/wayeb-0.2.0-SNAPSHOT.jar recognition --fsm:results/myFSM.fsm --stream:results/myFSM.fsm --isKafka:true --domainSpecificStream:maritime --statsFile:results/myFSM
```

* Start Wayeb before the stream simulation so you won't miss any events

* For the  *stream* argument we have to specify a random existing file because it's a required argument. It won't be used though as the engine will read the stream from Kafka

* A *terminate* message needs to be send for Wayeb to stop reading from the topic and produce statistics. ui.StreamSimulator automatically appends this message at the end of the simulated string.

#Configuration Files

Wayeb reads from the *defaultTopic* by default. If we want to change this as well as other parameters we can use a configuration file. An example config file exists in *kafkaConfigs/kafkaEarliest.properties*:

```
java -jar cef/target/scala-2.12/wayeb-0.2.0-SNAPSHOT.jar recognition --kafkaConf:kafkaConfigs/kafkaEarliest.properties --fsm:results/myFSM.fsm --stream:results/myFSM.fsm --isKafka:true --domainSpecificStream:maritime --statsFile:results/myFSM
```

* You can write your own config files inside the *kafkaConfigs/* folder. They will be ignored by git
* This configuration makes Wayeb read from the very first record of the topic (auto.offset.reset=earliest) as opposed to the default *latest* that reads from the latest commited record.
* It will also keep start reading from this very first record whenever it restarts as no records are committed (enable.auto.commit=false).
* The point of this configuration is to store a stream on kafka and then read it as many times as you want in a batch like way. Note that no simulation happens and messages are received instantly as the stream is already stored