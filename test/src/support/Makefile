JVM_VERIFIER_DIR=../../..
JVM_SUPPORT_DIR=$(JVM_VERIFIER_DIR)/support

all :
	javac -g -target 1.6 -source 1.6 -cp .:$(JVM_VERIFIER_DIR)/support/galois.jar:$(JVM_VERIFIER_DIR)/jars/bcprov-jdk16-145.jar *.java

%.class : %.java
	javac -g -target 1.6 -source 1.6 -cp .:$(JVM_VERIFIER_DIR)/support/galois.jar:$(JVM_VERIFIER_DIR)/jars/bcprov-jdk16-145.jar $<
