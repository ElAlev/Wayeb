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

 

## Event streams

We currently use either files or Kafka topics to represent input streams. 
See [stream](cef/src/main/scala/stream) package for all relevant code.

### Streams from files

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


### Streams from Kafka

See [Reading streams from Kafka](/docs/kafkainput.md).

## Complex event definitions

Definitions for complex events should be provided in a file.
Such definitions should be written as symbolic regular expressions (SRE) 
or symbolic regular expressions with memory and output (SREMO),
with prefix notation.

### SRE

Each pattern is a symbolic regular expression, 
i.e., it is composed of a regular part and a logic part.

The operators for the regular part are those of standard regular expressions:
* concatenation/sequence, denoted by ;
* disjunction, denoted by +
* Kleene-star/iteration, denoted by *
* Negation/complement is also available, denoted by !.

Regular expressions are written using prefix notation. 
For example, 
`;(a,*(+(b,c)),d)` 
means an a, 
followed by zero or more bs or cs 
followed by a d.

Three selection strategies available:
* Strict contiguity. Default strategy, does not have a special symbol.
* Skip-till-any, denoted by #.
* Skip-till-next, denoted by @.

When applied to disjunction or negation, selection strategies have no effect.

In symbolic regular expressions, 
terminal symbols (e.g., a,b,c, etc.) are replaced with Boolean expressions.
A Boolean expression is either an atomic or a complex sentence.
An atomic sentence is a predicate, 
possibly with arguments and each argument is a term.
Predicate names are strings starting with uppercase letters, e.g., `ThisIsAPredicate`.
Terms may be:
* Logic Constants, as strings starting with uppercase letters, e.g., ThisIsAConstant.
* Numerical Constants, as (possibly negated) decimals, e.g. 4.0 or -13.666.
* Variables, as strings starting with lowercase letters, e.g., thisIsAVariable.

Note that variables should not be used currently. To be supported in future versions.
A complex sentence is composed of atomic sentences 
combined through the logical operations of:
* Conjunction/AND, denoted by ^
* Disjunction/OR, denoted by |
* Negation, denoted by -

Again, prefix notation is used.
 or example, if `P` and `Q` are predicates, 
 then `|(P,-Q)` is the complex sentence (`P` or not `Q`).

Each pattern may be followed by:
* The assumed order of the Markov chain, denoted by a positive integer (or zero) inside {}. Do not use if you intend to run only recognition.
* A partition attribute, denoted by a string starting with a lowercase letter inside {}.
* A window, denoted by a positive integer inside {}.
* A window type, denoted by a string, either 'time' or 'count',  inside {}. To be used only if a window has also been used. 

If a partition attribute exists,
this means that the stream will be split into sub-streams according to the value of the attribute
that each event has. 
For example, 
in the maritime domain we may need to apply a pattern on a per-vessel basis.
We thus designate the vessel's MMSI (id) as the partition attribute.
This attribute must exist in all the input events.

A window imposes the extra constraint that the 'duration' of every complex event must not exceed the window value.
There are two types of windows:
* Count-based ones. The 'duration' of a complex event in this case is just the number of its input events. For a match to be a valid complex event, the number of its input events must not exceed the window value.
* Time-based ones. The 'duration' of a complex event is the difference between the timestamps of its last and first input events. Each input event must have a 'timestamp' attribute.  
If no window type is provided, then a count-based window is used.
Windows are required only for NSRA and DSRA.

A file may contain multiple patterns, separated by &.

Full example,

````
;(IsEventTypePredicate(A),IsEventTypePredicate(B)){order:2}
&
;(|(IsEventTypePredicate(A),IsEventTypePredicate(B)),IsEventTypePredicate(C)){order:0}{partitionBy:someAttribute}{window:10}{windowType:count}
````

IsEventTypePredicate just checks for the event type and is already implemented
(see [fsm.symbolic.logic.predicates.IsEventTypePredicate](cef/src/main/scala/fsm/symbolic/logic/predicates/IsEventTypePredicate.scala)).

Note that for each predicate in a pattern, 
there must exist an implementation with the same name under
[fsm.symbolic.logic.predicates](cef/src/main/scala/fsm/symbolic/logic/predicates).

### SREMO

SREMO are an extension of SRE.
Thus, the syntax for SRE described above also holds for SREMO.

SRE are enriched with register variables in order to write SREMO.
Register variables are used in two ways. 

First, they must be declared. Each sentence may be accompanied at the end by a register variable, provided
as a string literal. 
For example, 
````
IsEventTypePredicate(A)["x"] 
````
denotes an atomic sentence (IsEventTypePredicate(A)) and x is the register variables. 
Register variables must be declared immediately after a sentence and must be written inside brackets and 
double quotes ([""]). A register variable declaration implies that the event triggering the sentence must be stored in 
a register with the corresponding name. A register variable may not be declared with the same name multiple times.

Second, a register variable may be referred to in the arguments list of an atomic sentence (other than the one in
which it was declared). For example,
````
EQAttrStr(EventType,"x")
````
is an atomic sentence whose second argument is the register variable x. 
Register variables in an argument list must be provided inside double quotes. 
The meaning is that the atomic sentence can have access to the contents of this register during its evaluation. 
For example,
EQAttrStr(EventType,"x") evaluates to true iff the event type of the current event is equal to the event type of the
event that has been store in the register x
(see [fsm.symbolic.logic.predicates.EQAttrStr](cef/src/main/scala/fsm/symbolic/logic/predicates/EQAttrStr.scala)).
Any predicates handling register variables in their arguments must also have been implemented 
under [fsm.symbolic.logic.predicates](cef/src/main/scala/fsm/symbolic/logic/predicates). 

SREMO should also be accompanied by a window, given as a natural integer at the end of the SREMO.
For example, 
````
;(IsEventTypePredicate(A)["x"],EQAttrStr(EventType,"x")){order:1}{window:2} 
````
is a SREMO with a (count-based) window of length 2. 
Note that if a window is not provided, it will be set to a default value of 0, which effectively renders
the SREMO unsatisfiable.

## Declarations

Relevant only for forecasting. Ignore if you are interested only in recognition.

Besides the complex event patterns themselves,
there is another input file that may be provided to Wayeb when converting a pattern to an automaton.
This is a file containing certain *declarations* that may help in optimizing the automaton 
(reduce the number of its transitions and states).

Each declaration line can be:
* Sets of extra predicates to be taken into account when building the SDFA and the Markov chain that do not
appear in the pattern itself but may be important features for forecasting. 
Extra predicates are given in parentheses, preceded by `+`.
For example, 
if a pattern on moving objects has a single predicate like `SpeedGreaterThan(20)`, 
we might also want to add other predicates, like `SpeedLowerThan(5)` or `SpeedBetween(5,20)`.
We then write 
````+(SpeedLowerThan(5),SpeedBetween(5,20))````.
* Sets of mutually exclusive predicates that help in reducing the size of the SDFA. 
Exclusive predicates are given in parentheses preceded by `~`. 
From all predicates inside such parentheses at most one may evaluate to `TRUE`.
For example, 
we could write 
`~(SpeedLowerThan(5),SpeedBetween(5,20))`. 
This means that we can avoid creating all
the (4) min-terms between these two predicates. 
Make sure that the predicates are indeed mutually exclusive,
otherwise unintended semantics may be introduced.

Different sets of extras/exclusives given in separate lines.
