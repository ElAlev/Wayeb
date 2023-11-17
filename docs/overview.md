# Overview

Wayeb is a CEP/F engine that consumes streams of events 
and produces complex events and forecasts for complex events,
according to a set of given patterns, i.e., complex event definitions.

Each pattern is converted to an automaton and this automaton can be used to detect complex event matches.
If you are only interested in running recognition, 
then this automaton is enough.

If you are interested in forecasting as well, 
then the automaton must be converted to a Markov model (full- or variable-order).

Wayeb has its own language for expressing patterns
(to be described later in more details).
Patterns are written either as symbolic regular expressions (SRE) 
or as symbolic regular expressions with memory and output (SREMO). 
The difference is that SREMO allow the user to write relational patterns
(i.e., patterns where simple events may be related through constraints).
For this reason,
in SREMO the user must declare (register) variables where simple events are stored.
Note that SREMO are a superset of SRE.
A SRE is by definition also a SREMO,
without any relational constraints.
Thus, the same compiler is used in both cases.

SRE may be converted to:
* non-deterministic symbolic finite automata (NSFA), or
* deterministic symbolic finite automata (DSFA).

SREMO may be converted to:
* non-deterministic symbolic register automata (NSRA), or
* deterministic symbolic register automata (DSRA).

In the general case,
symbolic automata (either NSFA or DSFA) are more efficient than symbolic register automata.
Thus, if you do not really need relational constraints in your patterns,
stick to SRE and symbolic automata.

Wayeb can perform recognition with either SRE (NSFA or DSFA) or SREMO (NSRA or DSRA).
The difference between non-deterministic and deterministic automata is that the former allow the engine 
to enumerate all complex events upon detection,
i.e., it can report, for each detected complex event, the simple events from which it is composed.
The engine can create multiple runs for each pattern and each run can track each (partial or full) match.
This is not possible with deterministic automata,
which, by definition, have only a single run.
They can report when a (at least one) complex event has been detected,
but cannot report its simple constituting events.
On the other hand,
deterministic automata,
exactly because they have only a single run,
are more efficient.
Thus, if you are not interested in enumerating the detected complex events,
stick to the deterministic automata.

As far as forecasting is concerned,
only deterministic automata may be used.
The current forecasting methods,
based on Markov models,
require deterministic automata.
Note though that forecasting with DSRA,
although supported in principle,
is very demanding (both memory- and time- wise) and is not recommended.

SRE and SREMO may also be accompanied by a window constraint.
Window constraints are optional for NSFA and DSFA.
They are required for NSRA and DSRA.
Even though windows are optional for NSFA,
it is recommended that you use one,
as it is possible that windowless NSFA may keep creating runs without terminating them.

Selection strategies are also supported:
* strict-contiguity, which is the default strategy.
* skip-till-any. This strategy may be used with all automaton models.
* skip-till-next. Cannot be currently used with NSRA, since it relies on negation/determinization. Could be used (in the future) with windowed NSRA. 

The following table summarizes the options a user has with the various automaton models.


|                                                                                                                                                                            |          NSFA          |     DSFA      |      NSRA       |      DSRA       |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:----------------------:|:-------------:|:---------------:|:---------------:|
|                                                                            <strong>SRE</strong>                                                                            |          yes           |      yes      | not recommended | not recommended |
|                                                                           <strong>SREMO</strong>                                                                           |           no           |      no       |       yes       |       yes       |
|                                                                        <strong>Recognition</strong>                                                                        |          yes           |      yes      |       yes       |       yes       |
|                                                                        <strong>Forecasting</strong>                                                                        |           no           |      yes      |       no        | not recommended |
|                                                                        <strong>Efficiency</strong>                                                                         |         lower          |    higher     |      lower      |     higher      |
|                                                                          <strong>Windows</strong>                                                                          | optional (recommended) |   optional    |    required     |    required     |
|                                                                     <strong>strict-contiguity</strong>                                                                     |     yes (default)      | yes (default) |  yes (default)  |  yes (default)  |
|                                                                       <strong>skip-till-any</strong>                                                                       |          yes           |      yes      |       yes       |       yes       |
|                                                                      <strong>skip-till-next</strong>                                                                       |          yes           |      yes      |       no        |       yes       |

 

