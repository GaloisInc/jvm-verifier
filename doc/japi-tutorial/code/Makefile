USER_CP=../../../user
BC_JAR=${USER_CP}/bcprov-jdk16-145.jar
SUPPORT_CP=../../../support/galois.jar
JDK_CP=../../../jdk1.6
JSS=../../../dist/build/jss/jss
CSS=../../../dist/build/css/css
ABC=abc
CRYPTOL=cryptol

all: JavaSHA384.aig JavaMD5.aig MD5-ref.aig
#SHA384-ref.aig

%.class: %.java
	javac -g -cp ${USER_CP}:${SUPPORT_CP}:${BC_JAR} $<

%.aig: %.class
	${JSS} -c ${USER_CP}:${JDK_CP}:. -j ${SUPPORT_CP} $*

%.cnf: %.class
	${JSS} -c ${USER_CP}:${JDK_CP}:. -j ${SUPPORT_CP} $*

eval: JavaMD5Eval.class
	${JSS} -c ${USER_CP}:${JDK_CP}:. -j ${SUPPORT_CP} JavaMD5Eval

#SHA384-ref.aig: SHA2.cry gen-sha384-fm.cmd
#	cryptol -b gen-sha384-fm.cmd

MD5-ref.aig: MD5.cry
	css -o $@ $< md5_ref

eqabc: MD5-ref.aig JavaMD5.aig
	echo "cec $^"  | ${ABC}
