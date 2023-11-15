# Recognition

For running recognition with a given pattern,
we must first convert the pattern into an automaton.

## With DSFA

In order to create an automaton from an example maritime pattern, 
run the following:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar compile --patterns:$WAYEB_HOME/patterns/maritime/port/pattern.sre --declarations:$WAYEB_HOME/patterns/maritime/port/declarationsDistance1.sre --fsmModel:dsfa --outputFsm:$WAYEB_HOME/results/tmpdsfa.fsm
````
You must specify: 
* the path to the file with the patterns;
* the path to the declarations file;
* the FSM (finite state machine) model;
* and the path to the file containing the serialized automaton.

After compiling the pattern into a DSFA (deterministic symbolic finite automaton),
you can run recognition on a given stream of events with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar recognition --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --fsmModel:dsfa --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/recstats
````
You must specify: 
* the path to the file with the serialized automaton;
* the FSM (finite state machine) model;
* the path to the file with the stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to a file with the statistics gathered.

## With NSFA

If you are interested only in recognition,
you can use a different FSM model.
There are 4 valid FSM models:
* dsfa (deterministic symbolic finite automata).
* nsfa (non-deterministic symbolic finite automata).
* dsra (deterministic symbolic register automata).
* nsra (non-deterministic symbolic register automata).

For this example pattern,
which has no relational constraints,
we can use dsfa or nsfa.
You can run the following in order to construct a NSFA from the pattern:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar compile --patterns:$WAYEB_HOME/patterns/maritime/port/pattern.sre --declarations:$WAYEB_HOME/patterns/maritime/port/declarationsDistance1.sre --fsmModel:nsfa --outputFsm:$WAYEB_HOME/results/tmpnsfa.fsm
````
You can run recognition with this non-deterministic automaton with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar recognition --fsm:$WAYEB_HOME/results/tmpnsfa.fsm --fsmModel:nsfa --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/recstats
````
Note that the matches now contain exactly the indices of the input events.
In the case of DSFA, each match reports multiple redundant input events,
essentially breaking the input stream into as many sub-streams as the number of complex events.
Please, also note that the index of each input event is created automatically by the engine.
It is just an increasing unique identifier 
(basically, the line number in the file if the stream is provided in a file).

## With NSRA

Let's run now recognition with a pattern having relational constraints
(`$WAYEB_HOME/patterns/maritime/port/patternRel.sre`).
We will need to use NSRA.
Run the following to construct a NSRA from this pattern:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar compile --patterns:$WAYEB_HOME/patterns/maritime/port/patternRel.sre --fsmModel:nsra --outputFsm:$WAYEB_HOME/results/tmpnsra.fsm
````
You can run recognition with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar recognition --fsm:$WAYEB_HOME/results/tmpnsra.fsm --fsmModel:nsra --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/recstats
````
Notice that we do not provide a declarations file in this case,
since we will not be using the NSRA for forecasting later. 

## With optimizations

If you are running recognition with NSFA or NSRA,
you may also enable certain optimizations which increase the throughput of the engine,
by setting the `opt` argument to `true`.
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar recognition --fsm:$WAYEB_HOME/results/tmpnsra.fsm --fsmModel:nsra --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/recstats --opt:true
````
CAUTION: This option should be used only with a single pattern (no multiple patterns in the .sre file) 
and without any partition attributes.
There is no theoretical reason for these limitations.
It is an engineering issue which will be addressed in a future version.


