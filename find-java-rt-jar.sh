#! /bin/bash

# Find the Java core classes JAR, 'rt.jar' (called 'classes.jar' on OS
# X for JDKs prior to 1.7).

JDK=$(java -verbose 2>&1 | sed -n -e '1 s/\[Opened \(.*\)\]/\1/p')
if [ -z "$JDK" ]; then
    echo "Failed to locate Java core classes (rt.jar / classes.jar)!" >&2
    echo "The trick used by this script is only known to work with" >&2
    echo "Sun-based JDKs." >&2
    exit 1
fi
echo -n $JDK
