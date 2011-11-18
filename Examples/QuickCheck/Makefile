SAWSCRIPT=../../dist/build/sawScript/sawScript
CLASSES_JAR=../../jdk1.6/classes.jar

EXAMPLES:=$(patsubst %.saw,%.run,$(wildcard *.saw))

.PHONY: all

all: $(EXAMPLES)
	echo $(EXAMPLES)

%.run : %.saw Example.class
	-$(SAWSCRIPT) -c. -j$(CLASSES_JAR) $<


Example.class: Example.java
	javac Example.java


