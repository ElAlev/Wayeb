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
If you wan to skip the unit tests, run the command:
```
$ sbt assembly
```

If $WAYEB_HOME is the root directory of Wayeb, 
then the fat jar will be located under $WAYEB_HOME/cef/target/scala-2.12,
with the name wayeb-0.3.0-SNAPSHOT.jar.  
This is a self-contained jar containing everything you might need to run Wayeb.
You can copy it wherever you want.

If you want to run the experiments described in **DBLP:journals/vldbj/AlevizosAP20**
(see [How to cite Wayeb](docs/references.md)),
then you also need to set $WAYEB_HOME as an environment variable.
For example, 
````
$ export WAYEB_HOME=/root/dir/to/Wayeb
````
If you want to permanently set $WAYEB_HOME, 
you may want to add this line to your .profile or .bashrc files.

To see the options available when running Wayeb,
run the following command:
````
$ java -jar wayeb-0.6.0-SNAPSHOT.jar --help | more
````
