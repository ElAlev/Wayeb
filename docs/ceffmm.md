# Complex Event Forecasting with full-oder Markov models


If you want to run forecasting with full-order Markov models,
then the first step is to have compiled your pattern into an automaton,
as described in [Recognition](docs/cep.md).
Specifically,
you need to have converted your pattern into a deterministic symbolic finite automaton (DSFA).
Make sure that tha pattern,
before being compiled, 
is written with the order that you want,
as described in [Overview](docs/overview.md).

## Training

After the automaton has been constructed,
you need to run maximum likelihood estimation (MLE) to learn the Markov chain's transition matrix
from a given training stream. 

You run MLE with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar mle --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --outputMc:$WAYEB_HOME/results/tmp.mc
````
You must specify:
* the path to the file with the serialized automaton;
* the path to the file with the training stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to the file with the serialized learnt Markov chain.

## Testing (default settings)

Now you may run forecasting on a test event stream with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar forecasting --modelType:fmm --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --mc:$WAYEB_HOME/results/tmp.mc --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
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

CAUTION: do not use DSRA with full-order Markov models. Stick to DSFA.

## Testing (alternative settings)

If we run forecasting as described above,
the engine runs with its default settings.
Specifically, it runs REGRESSION-INTERVAL,
i.e., it tries to emit an interval (in terms of number of future events) within which a complex event
is expected to occur. 
However, there are other forecasting types available
(see also the 2022 VLDBJ paper *Complex Event Forecasting with Prediction Suffix Trees*).
Currently, the forecasting types/methods exposed to the CLI are the following:
* REGRESSION-INTERVAL;
* REGRESSION-ARGMAX;
* CLASSIFICATION-NEXTK.

### REGRESSION-ARGMAX

With REGRESSION-ARGMAX, the engine tries to forecast a future interval where a complex event is expected.
This interval is built around the (single) most probable future point.
For example,
you may run the following
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar forecasting --foreMethod:argmax --horizon:30 --maxSpread:5 --modelType:fmm --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --mc:$WAYEB_HOME/results/tmp.mc --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
````
You additionally must specify:
* the forecasting method (argmax in this case);
* the horizon, i.e., the total number of points for the waiting-time distribution;
* the maximum spread of the interval around the most probable point, i.e., its length.

### REGRESSION-INTERVAL

REGRESSION-INTERVAL is the default method. 
If you do not specify a forecasting method,
this one will be used.
With REGRESSION-INTERVAL, the engine tries again to forecast a future interval where a complex event is expected.
However, the interval need not be centered around the most probable future point.
It may be located anywhere along the waiting-time distribution.
There are two constraints.
First, the interval's probability must exceed a given confidence threshold.
Second, among all intervals exceeding this threshold,
the predicted one must be the shortest.
For example,
you may run the following
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar forecasting --foreMethod:smart-scan --horizon:30 --threshold:0.7 --maxSpread:5 --modelType:fmm --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --mc:$WAYEB_HOME/results/tmp.mc --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
````
You additionally must specify:
* the forecasting method (smart-scan in this case). 
The name derives from the fact that it scans the distribution efficiently (not exhaustively) to find the interval;
* the horizon, i.e., the total number of points for the waiting-time distribution; 
* the confidence threshold;
* the maximum spread of the interval. If all valid intervals exceed in length this value, no forecast is emitted. 

### CLASSIFICATION-NEXTK

With CLASSIFICATION-NEXTK, 
the engine emits binary forecasts.
Each forecast lets us know whether a complex event is expected to occur within the next k points,
with probability above a given threshold.
For example,
you may run the following
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar forecasting --foreMethod:classify-nextk --horizon:30 --threshold:0.3 --maxSpread:5 --modelType:fmm --fsm:$WAYEB_HOME/results/tmpdsfa.fsm --mc:$WAYEB_HOME/results/tmp.mc --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
````
You additionally must specify:
* the forecasting method (classify-nextk in this case);
* the horizon, i.e., the total number of points for the waiting-time distribution;
* the confidence threshold;
* the maximum spread, which, in this case is interpreted as the length k of the future window.
