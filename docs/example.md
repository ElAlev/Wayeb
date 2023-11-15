# Running example for Wayeb with maritime data

Clone Wayeb. Let's assume Wayeb's Home is $WAYEB_HOME.

```
cd $WAYEB_HOME
sbt
assembly
```

This will create a fat jar under *$WAYEB_HOME/target/scala-2.11/wayebcli.jar*. 
You can use this jar to run Wayeb.

In order to create a SDFA (symbolic deterministic finite automaton) from a pattern, 
run the following:

`java -jar wayebcli.jar fsmDisambiguate --fsmType:symbolic --patterns:$WAYEB_HOME/patterns/datacron/maritime/approachingBrestPort/approachingBrestPort.cepl --declarations:$WAYEB_HOME/patterns/datacron/maritime/approachingBrestPort/approachingBrestPortDeclNoExtras.cepl --outputFsm:$WAYEB_HOME/approachingBrestPort0.fsm`

This will create a SDFA for the pattern *approachingBrestPort.cepl* as a serialized object named 
*approachingBrestPort0.fsm*.
If you want to run recognition using this SDFA, you may run the following:

`java -jar wayebcli.jar recognition --fsmType:symbolic --fsm:$WAYEB_HOME/approachingBrestPort0.fsm --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:datacronMaritime --streamArgs: --statsFile:$WAYEB_HOME/approaching.stats`

This will use the (serialized) SDFA created in the previous step and the sample stream 
*$WAYEB_HOME/data/maritime/227592820.csv* to perform recognition. 
Some stats will be printed and written to *$WAYEB_HOME/approaching.stats*.

You can do the above from within IntelliJ as well.  
Just go to Run->Edit Configurations, create two new configurations, one for disambiguation and one for recognition and
as program arguments give  

`fsmDisambiguate --fsmType:symbolic --patterns:$WAYEB_HOME/patterns/datacron/maritime/approachingBrestPort/approachingBrestPort.cepl --declarations:$WAYEB_HOME/patterns/datacron/maritime/approachingBrestPort/approachingBrestPortDeclNoExtras.cepl --outputFsm:$WAYEB_HOME/approachingBrestPort0.fsm`

and

`recognition --fsmType:symbolic --fsm:$WAYEB_HOME/approachingBrestPort0.fsm --stream:$WAYEB_HOME/data/maritime/227592820.csv --domainSpecificStream:datacronMaritime --streamArgs: --statsFile:$WAYEB_HOME/approaching.stats`

respectively.



compile --patterns:/home/work/src/Wayeb/patterns/maritime/port/pattern.sre --declarations:/home/work/src/Wayeb/patterns/maritime/port/declarationsDistance1.sre --outputFsm:/home/work/src/Wayeb/results/tmp.fsm

recognition --fsm:/home/work/src/Wayeb/results/tmp.fsm --stream:/home/work/src/Wayeb/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:/home/work/src/Wayeb/results/recstats

mle --fsm:/home/work/src/Wayeb/results/tmp.fsm --stream:/home/work/src/Wayeb/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --outputMc:/home/work/src/Wayeb/results/tmp.mc

forecasting --modelType:fmm --fsm:/home/work/src/Wayeb/results/tmp.fsm --mc:/home/work/src/Wayeb/results/tmp.mc --stream:/home/work/src/Wayeb/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:/home/work/src/Wayeb/results/forestats

java -jar wayeb-0.3.0-SNAPSHOT.jar learnSPST --patterns:/home/work/src/Wayeb/patterns/maritime/port/pattern.sre --declarations:/home/work/src/Wayeb/patterns/maritime/port/declarationsDistance1.sre --stream:/home/work/src/Wayeb/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --outputSpst:/home/work/src/Wayeb/results/tmp.spst

forecasting --modelType:vmm --fsm:/home/work/src/Wayeb/results/tmp.spst --stream:/home/work/src/Wayeb/data/maritime/227592820.csv --domainSpecificStream:maritime --streamArgs: --statsFile:/home/work/src/Wayeb/results/forestats  