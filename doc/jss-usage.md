% JSS(1) JSS User Guide
% Galois, Inc
% October 1, 2014

NAME
====

jss - Java symbolic simulator

SYNOPSIS
========

jss [*OPTIONS*] [*CLASS NAME*]

DESCRIPTION
===========

The Java Symbolic Simulator, jss, interprets a Java class file but
treats certain designated program variables as arbitrary, symbolic
expressions rather than concrete values. Therefore, the resulting
values of certain program variables may be described as symbolic
expressions in terms of the initial values of symbolic input values.

The symbolic expressions representing the values of particular program
variables can be stored in the form of And-Inverter Graphs (AIGs) for
further processing by external tools.

Example usage:

    jss -c . -j /usr/java/jdk1.6.jar:/path/to/galois.jar Main

will simulate the function `main()` from the class file `Main.class`
in the current directory, and expect to find additonal support classes
in the supplied jar files.

OPTIONS
=======

-b, \--blast
:   Always bitblast symbolic condition terms at branches.  This may
    force symbolic termination, but can incur a simulator performance
    cost.

-c *PATH*, \--classpath=*PATH*
:   A colon-delimited list of directories to search for class files.
    Use semicolon delimiters on Windows.

-d *INT*, \--dbug[=*INT*]
:   Specify the verbosity level for debugging messages. Valid values
    are in the range 0-5.

-e, \--errpaths
:   Print details of symbolic exeuction paths that end in errors

-j *JARS*, --jars=*JARS*
:   A colon-delimited list of jar files to search for class files.  The
    jdk1.6 jar be explicitly given using this option.  Use semicolon
    delimiters on Windows.

    NB: jss expects to find the file `galois.jar` in the same directory
    as the jss executable itself.  If this is not the case on your
    system, you may use this option to specify an alternate location
    for the file.

-o *ARGS*, \--opts=*ARGS*
:   Specify the space-delimited list of arguments to pass to the
    main() method.

-u, \--usesaw
:   Use the SAWCore backend instead of the default word backend

-x, \--xlate
:   Print the symbolic AST translation and exit

\--startdebugger
:   Break and enter the JSS debugger when running the main method

-V, \--version
:   Print out the version of the simulator.

-?, \--help
:   Print a help message.

SEE ALSO
========

`cryptol` (1).

`lss` (1).

`sawscript` (1).
