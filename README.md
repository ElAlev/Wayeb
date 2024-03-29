# Wayeb

Wayeb is a Complex Event Processing and Forecasting (CEP/F) engine written in [Scala](http://scala-lang.org).
It is based on symbolic automata and full- or variable-order Markov models.

## Updates

- 2023/11/15
  - Wayeb has been upgraded to version 0.6.0.
  - Released code for variable-order Markov models.
  - Released code for symbolic regular expressions with memory and output (SREMO) and symbolic register transducers (SRT).

## Quick start

### Building

Assuming $WAYEB_HOME is the root directory of Wayeb,
then go inside:

```
$ cd $WAYEB_HOME
```

Let's build a fat jar:

```
$ sbt assembly
```

### Recognition

In $WAYEB_HOME/data/demo/data.csv you may find a very simple dataset,
consisting of 100 events. The event type is either A, B or C.
In $WAYEB_HOME/patterns/demo/a_seq_b_or_c.sre you may find a simple complex event definition for the above dataset.
It detects an event of type A followed by another event of type B or C.
If we want to run this pattern over the stream,
we must first compile this pattern into an automaton
(make sure you have created a *results* folder under $WAYEB_HOME):
```
$ java -jar cef/target/scala-2.12/wayeb-0.6.0-SNAPSHOT.jar compile --patterns:patterns/demo/a_seq_b_or_c.sre --declarations:patterns/demo/declarations.sre --outputFsm:results/a_seq_b_or_c.fsm
```
Now, *results/a_seq_b_or_c.fsm* is the produced serialized finite state machine.
Note that we also provided as input a *declarations.sre* file.
This file simply lets the engine know that the three predicates *IsEventTypePredicate(A)*, *IsEventTypePredicate(B)* and *IsEventTypePredicate(C)*
are mutually exclusive (i.e., an event can have only one type).
This helps the compiler create a more compact automaton.
We can use this FSM to perform event recognition on this simple dataset:
```
$ java -jar cef/target/scala-2.12/wayeb-0.6.0-SNAPSHOT.jar recognition --fsm:results/a_seq_b_or_c.fsm --stream:data/demo/data.csv --statsFile:results/recstats
```

### Forecasting

For forecasting, we first need to use a training dataset in order to learn a probabilistic model for the FSM.
For this simple guide,
we will use $WAYEB_HOME/data/demo/data.csv both as a training and as a test dataset,
solely for convenience.
Normally, you should use different datasets.

We first run maximum likelihood estimation:
```
$ java -jar cef/target/scala-2.12/wayeb-0.6.0-SNAPSHOT.jar mle --fsm:results/a_seq_b_or_c.fsm --stream:data/demo/data.csv --outputMc:results/a_seq_b_or_c.mc
```
The file *results/a_seq_b_or_c.mc* is the serialized Markov model.
The final step is to use the FSM and the Markov model to perform forecasting:
```
$ java -jar cef/target/scala-2.12/wayeb-0.6.0-SNAPSHOT.jar forecasting --modelType:fmm --fsm:results/a_seq_b_or_c.fsm --mc:results/a_seq_b_or_c.mc --stream:data/demo/data.csv --statsFile:results/forestats --threshold:0.5 --maxSpread:10 --horizon:20 --foreMethod:classify-nextk
```

## License

Copyright (c) Elias Alevizos

Wayeb comes with ABSOLUTELY NO WARRANTY.

Wayeb follows a dual licensing scheme.

For use by individuals,
Wayeb is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
This license is provided exclusively for research purposes.
The results of any such research involving Wayeb must be made publicly available.

For commercial/institutional/governmental use or any other use by private or public
legal entities, sharing, modifying and distributing Wayeb or any derivatives of it
in any form, such as source code, libraries and executables, requires the written
permission of its author(s) (Elias Alevizos) and a possible request for licensing fees.

## Documentation

- [Building](docs/building.md)
- [Overview](docs/overview.md)
- [Wayeb language](docs/lang.md)
- [Representing streams of input events](docs/streams.md)
- [Recognition](docs/cep.md)
- [Forecasting with full-oder Markov models](docs/ceffmm.md)
- [Forecasting with variable-oder Markov models](docs/cefvmm.md)
- [Using Wayeb as a library](docs/lib.md)
- [How to cite Wayeb](docs/references.md)

## Citing Wayeb
If you want to cite Wayeb, use the following reference:
```
@article{DBLP:journals/vldb/AlevizosAP22,
  author       = {Elias Alevizos and
                  Alexander Artikis and
                  Georgios Paliouras},
  title        = {Complex event forecasting with prediction suffix trees},
  journal      = {{VLDB} J.},
  volume       = {31},
  number       = {1},
  pages        = {157--180},
  year         = {2022}
}
```
This paper describes a version of Wayeb that works with symbolic automata and variable-order Markov models.

The version that works with symbolic register automata and variable-order Markov models is described here
(to be updated with a version presenting the use of symbolic register transducers):
```
@article{DBLP:journals/corr/abs-2110-04032,
author       = {Elias Alevizos and
Alexander Artikis and
Georgios Paliouras},
title        = {Symbolic Register Automata for Complex Event Recognition and Forecasting},
journal      = {CoRR},
volume       = {abs/2110.04032},
year         = {2021}
}
```

Older versions have been presented in the following papers:

(Version that works only with classical automata and full-order Markov models)
```
@inproceedings{DBLP:conf/debs/AlevizosAP17,
  author    = {Elias Alevizos and
               Alexander Artikis and
               George Paliouras},
  title     = {Event Forecasting with Pattern Markov Chains},
  booktitle = {Proceedings of the 11th {ACM} International Conference on Distributed
               and Event-based Systems, {DEBS} 2017, Barcelona, Spain, June 19-23,
               2017},
  pages     = {146--157},
  publisher = {{ACM}},
  year      = {2017},
  url       = {https://doi.org/10.1145/3093742.3093920},
  doi       = {10.1145/3093742.3093920}
} 
```

(Version that works with symbolic automata and full-order Markov models)
```
@inproceedings{DBLP:conf/lpar/AlevizosAP18,
  author    = {Elias Alevizos and
               Alexander Artikis and
               Georgios Paliouras},
  editor    = {Gilles Barthe and
               Geoff Sutcliffe and
               Margus Veanes},
  title     = {Wayeb: a Tool for Complex Event Forecasting},
  booktitle = {{LPAR-22.} 22nd International Conference on Logic for Programming,
               Artificial Intelligence and Reasoning, Awassa, Ethiopia, 16-21 November
               2018},
  series    = {EPiC Series in Computing},
  volume    = {57},
  pages     = {26--35},
  publisher = {EasyChair},
  year      = {2018},
  url       = {https://easychair.org/publications/paper/VKP1}
}

```

## Contributors

* Elias Alevizos (main developer).
* [Emmanouil Ntoulias](https://github.com/manosntoulias) (stream simulator).
* Maria Petropanagiotaki (relaxed selection policies).
* [Evangelos Michelioudakis](https://github.com/vagmcs) (sbt structure).
