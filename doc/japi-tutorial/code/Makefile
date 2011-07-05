USER_CP=../../../user
SUPPORT_CP=../../../support/galois.jar
JDK_CP=../../../jdk1.6
JSS=../../../dist/build/jss/jss
ABC=abc
CRYPTOL=cryptol

all: JavaSHA384.aig SHA384-ref.aig JavaMD5.aig MD5-ref.aig

%.class: %.java
	javac -cp ${USER_CP} $<

%.aig: %.class
	${JSS} -c ${USER_CP}:${JDK_CP}:. -j ${SUPPORT_CP} $*

eval: JavaMD5Eval.class
	${JSS} -c ${USER_CP}:${JDK_CP}:. -j ${SUPPORT_CP} JavaMD5Eval

SHA384-ref.aig: SHA2.cry gen-sha384-fm.cmd
	cryptol -b gen-sha384-fm.cmd

MD5-ref.aig: MD5.cry gen-md5-fm.cmd
	cryptol -b gen-md5-fm.cmd

eqabc: SHA384-java.aig SHA384-ref.aig
	echo "cec $^"  | ${ABC}

eqcry: compare.cry SHA384-java.aig
	${CRYPTOL} -n $< -f symbolic -c ":prove MatchesRef"
