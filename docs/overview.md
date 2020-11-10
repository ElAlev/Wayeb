# Overview

Wayeb is a CEP/F engine that consumes streams of events 
and produces complex events and forecasts for complex events,
according to a set of given patterns, i.e., complex event definitions.

Each pattern is converted to a symbolic automaton and this automaton can be used to detect complex event matches.
If you are only interested in running recognition, 
then this automaton is enough.

If you are interested in forecasting as well, 
then the automaton must be converted to a Markov model. 


## Event streams

We currently use either files or Kafka topics to represent input streams. 
See [stream](../cef/src/main/scala/stream) package for all relevant code.

### Streams from files

Each event is a row and typically has a timestamp, an event type and a unique id.
Any other attributes may also be present as a Map of attribute names to values
(see [stream.GenericEvent](../cef/src/main/scala/stream/GenericEvent.scala)).

The simplest possible way to represent a stream is to create a CSV file 
with each line corresponding to an event. 
The first column should contain the event type (as a String) and the second column the 
event's timestamp (as a Long).
For this simple case, a parser is already available and you do not need to do anything else
(see [stream.source.GenericCSVLineParser](../cef/src/main/scala/stream/source/GenericCSVLineParser.scala)).

If you need a stream of events with more attributes,
you can do so, 
but you need to write a parser in order for the engine to know how to covert each line to and event.
For an example from the maritime domain,
see [stream.domain.maritime.MaritimeLineParser](../cef/src/main/scala/stream/domain/maritime/MaritimeLineParser.scala).
If you do so,
then you need to also modify the function stream.StreamFactory.getDomainStreamSource
in [stream.StreamFactory](../cef/src/main/scala/stream/StreamFactory.scala) 
and add another option for your domain.
Then, whenever you run [recognition](cep.md) or forecasting with [full-order Markov models](cefvmm.md),
you need to specify the option *domainSpecificStream* (e.g., *--domainSpecificStream:maydomain*).

Alternatively, you may also represent a stream as a JSON file
(see [stream.source.JsonFileStreamSource](../cef/src/main/scala/stream/source/JsonFileStreamSource.scala)).
Each JSON attribute will be converted to an event attribute.


### Streams from Kafka

See [Running example for simulating a stream](simulator.md).
## Complex event definitions

Definitions for complex events should be provided in a file.
Such definitions should be written as symbolic regular expressions with prefix notation.

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
* The assumed order of the Markov chain, denoted by a positive integer (or zero) inside {}
* A partition attribute, denoted by a string starting with a lowercase letter inside {}

If a partition attribute exists,
this means that the stream will be split into sub-streams according to the value of the attribute
that each event has. 
For example, 
in the maritime domain we may need to apply a pattern on a per vessel basis.
We thus designate the vessel's MMSI (id) as the partition attribute.

A file may contain multiple patterns, separated by &.

Full example,

````
;(IsEventTypePredicate(A),IsEventTypePredicate(B)){order:2}
&
;(|(IsEventTypePredicate(A),IsEventTypePredicate(B)),IsEventTypePredicate(C)){order:0}{partitionBy:someAttribute}
````

IsEventTypePredicate just checks for the event type and is already implemented
(see [fsm.symbolic.sfa.logic.predicates.IsEventTypePredicate](../cef/src/main/scala/fsm/symbolic/sfa/logic/predicates/IsEventTypePredicate.scala)).

Note that for each predicate in a pattern, 
there must exist an implementation with the same name under
[fsm.symbolic.sfa.logic.predicates](../cef/src/main/scala/fsm/symbolic/sfa/logic/predicates).

## Declarations

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
