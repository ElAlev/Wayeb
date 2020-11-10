# Recognition

For running recognition with a given pattern,
we must first convert the pattern into an automaton

In order to create a SDFA (symbolic deterministic finite automaton) from an example maritime pattern, 
run the following:
````
$ java -jar wayeb-0.2.0-SNAPSHOT.jar compile --patterns:$WAYEB_HOME/patterns/maritime/port/pattern.sre --declarations:$WAYEB_HOME/patterns/maritime/port/declarationsDistance1.sre --outputFsm:$WAYEB_HOME/results/tmp.fsm
````
You must specify: 
* the path to the file with the patterns;
* the path to the declarations file;
* and the path to the file containing the serialized SDFA. 

After compiling the pattern into a SDFA,
you can run recognition on a given stream of events with the following command:
````
$ java -jar wayeb-0.2.0-SNAPSHOT.jar recognition --fsm:$WAYEB_HOME/results/tmp.fsm --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/recstats
````
You must specify: 
* the path to the file with the serialized SDFA;
* the path to the file with the stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to a file with the statistics gathered. 

See also the script [ui.demo.RunCLI](../cef/src/main/scala/ui/demo/RunCLI.scala).
