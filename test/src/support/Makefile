JVM_VERIFIER_DIR=../../..
JVM_SUPPORT_DIR=$(JVM_VERIFIER_DIR)/support

all :
	javac -g -cp .:$(JVM_VERIFIER_DIR)/support/galois.jar:$(JVM_VERIFIER_DIR)/user/bcprov-jdk16-145.jar *.java

%.class : %.java
	javac -g -cp .:$(JVM_VERIFIER_DIR)/support/galois.jar:$(JVM_VERIFIER_DIR)/user/bcprov-jdk16-145.jar $<
