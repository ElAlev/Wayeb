# Complex Event Forecasting with variable-oder Markov models

If you want to run forecasting with variable-order Markov models,
the first step is to learn a model for a given pattern from a given training stream.
No need to compile the pattern first.
You can do this by running the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar learnSPST --patterns:$WAYEB_HOME/patterns/maritime/port/pattern.sre --fsmModel:dsfa --declarations:$WAYEB_HOME/patterns/maritime/port/declarationsDistance1.sre --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --outputSpst:$WAYEB_HOME/results/tmp.spst
````
You must specify:
* the path to the file with the patterns;
* the FSM model (dsfa or dsra, the latter is not recommended);
* the path to the declarations file;
* the path to the file with the training stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to the learnt prediction suffix tree that is going to be serialized.

There are also some optional arguments which you may want to pass.
These are the hyperparameters for the tree learning algorithm
(see also the 2022 VLDBJ paper *Complex Event Forecasting with Prediction Suffix Trees*):
* pMin (pMin>0 and pMin<1.0). This is the symbol threshold. Symbols with lower probability are discarded.
* alpha (alpha>0 and alpha<1.0). Used to calculate the conditional threshold = (1 + alpha) * gammaMin.
The conditional on the expanded context must be greater than this threshold.
* gammaMin (gammaMin>0 and gammaMin<1.0). Used to calculate the conditional threshold = (1 + alpha) * gammaMin. 
The conditional on the expanded context must be greater than this threshold. Also used for smoothing.
* r (r>0). This is the likelihood ratio threshold.
Contexts are expanded if the probability ratio of the conditional on the expanded context by the 
conditional on the original context is greater than this threshold.

You can then run forecasting with the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar forecasting --modelType:vmm --fsmModel:dsfa --fsm:$WAYEB_HOME/results/tmp.spst --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:$WAYEB_HOME/results/forestats
````
You must specify:
* the model type (fmm or vmm). vmm in this case for the variable-order Markov model (prediction suffix tree) we learnt.
* the FSM model (dsfa or dsra, the latter is not recommended);
* the path to the learnt serialized prediction suffix tree.
* the path to the file with the test stream;
* the stream domain in case of csv files 
(whose values should simply be `json` in case of JSON files);
* `streamArgs:` is to be used if you need to pass any special arguments to the event stream parser;
* the path to the file with the generated statistics.

You may also specify alternative settings for forecasting,
as described in the section about forecasting with full-order Markov models.
See [CEF with FMM](docs/ceffmm.md).

Note that here we use tha same stream both for training and testing.
This is done for convenience.
You should normally use different streams.
