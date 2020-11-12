# Building Wayeb

## Requirements

In order to build Wayeb from the source code you need to have Java SE version 8 or higher and 
[SBT](http://www.scala-sbt.org/) installed in your system.
Java 8 is recommended.

## Building

To build Wayeb, run the following command:
```
$ sbt build
```

This will compile Wayeb, run all unit tests and then create a fat jar. 
If you want to skip the unit tests, run the command:
```
$ sbt assembly
```

If $WAYEB_HOME is the root directory of Wayeb, 
then the fat jar will be located under $WAYEB_HOME/cef/target/scala-2.12,
with the name wayeb-0.2.0-SNAPSHOT.jar.  
This is a self-contained jar containing everything you might need to run Wayeb.
You can copy it wherever you want.

To see the options available when running Wayeb,
run the following command:
````
$ java -jar wayeb-0.2.0-SNAPSHOT.jar --help | more
````
