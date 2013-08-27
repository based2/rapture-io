[![Build Status](https://travis-ci.org/propensive/rapture-io.png?branch=master)](https://travis-ci.org/propensive/rapture-io)
Rapture I/O
===========

Version 0.8.0 (beta) for Scala 2.10.2

Rapture is a general purpose I/O library for Scala.

Compilation
-----------

You can now use Maven to compile the project :

mvn clean scala:compile

to get the jar file :

mvn clean scala:compile package

Note : please leave the sources in /src and /test since the main developer uses command line & vi to edit them, not an IDE.

Rapture I/O can be compiled the "low-tech" way using a few familiar commands, or
you can use the "high-tech" rapture-build Makefile, which can simplify the
compilation and development process.

Whilst the first option requires more steps to complete, developers should
already be very familiar with the commands used in each step.  The second option
should be less effort, though the automation comes at the expense of
transparency.


Low-tech Compilation
--------------------

Using Scala 2.10.0 (or later), compile Rapture I/O as follows:

        mkdir rapture
        cd rapture
        git clone https://github.com/propensive/rapture-test.git test
        cd test
        mkdir bin
        scalac -d bin src/*.scala
        jar cf test.jar -C bin rapture
        cd ..
        git clone https://github.com/propensive/rapture-io.git io
        cd io
        mkdir bin
        scalac -cp ../test/test.jar -d bin src/*.scala
        jar cf io.jar -C bin rapture

To use Rapture I/O in your own projects, just include io.jar on your classpath
and import rapture.io._.


High-tech Compilation
---------------------

First, clone the git rapture-build and rapture-io repositories:

        git clone https://github.com/propensive/rapture-build.git rapture.io/build
        git clone https://github.com/propensive/rapture-io.git rapture.io/io

Then change into the rapture-io directory, and create the development
environment.  This will download the correct version of Scala, and other
development dependencies.

        cd rapture.io/io
        make environment
        make fetch

Finally, to build rapture-io.jar just type

        make

You can then use Rapture I/O in your own projects by including io.jar on your
classpath, and importing rapture.io._.  You can experiment using the Scala REPL
using

        make console


