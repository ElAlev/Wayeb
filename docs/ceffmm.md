# Complex Event Forecasting with full-oder Markov models

If you want to run forecasting with full-order Markov models,
then the first step is to have compiled your pattern into an automaton,
as described in [Recognition](docs/cep.md).
Make sure that tha pattern,
before being compiled, 
is written with the order that you want,
as described in [Overview](docs/overview.md).
After the automaton has been constructed,
you need to run maximum likelihood estimation (MLE) to learn the Markov chain's transtion matrix
from a given training stream. 

You run MLE with the following command:
````
$ java -jar wayeb-0.2.0-SNAPSHOT.jar mle --fsm:$WAYEB_HOME/results/tmp.fsm --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --outputMc:$WAYEB_HOME/results/tmp.mc
````
You must specify:
* the path to the file with the serialized automaton;
* the path to the file with the training stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to the file with the serialized learnt Markov chain.

Now you may run forecasting on a test event stream with the following command:
````
$ java -jar wayeb-0.2.0-SNAPSHOT.jar forecasting --modelType:fmm --fsm:$WAYEB_HOME/results/tmp.fsm --mc:$WAYEB_HOME/results/tmp.mc --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
````
You must specify:
* the model type (fmm or vmm). fmm in this case for the full-order Markov model we learnt.
* the path to the file with the serialized automaton;
* the path to the file with the serialized Markov chain;
* the path to the file with the test stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to the file with the generated statistics.

Note that here we use tha same stream both for training and testing.
This is done for convenience.
You should normally use different streams.

See also the script [ui.demo.RunCLI](cef/src/main/scala/ui/demo/RunCLI.scala).