all:
	(cd com/galois/core && make)
	(cd com/galois/symbolic && make)
	find com -name '*.class' | xargs jar cvf galois.jar
