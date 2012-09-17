rmdir /s build
mkdir build
javac -g -cp src;..\..\support src\com\galois\ecc\*.java -d build
javac -g -cp build;tests tests\com\galois\ecc\*.java -d build
cd saw
cd spec
cd sbv
make_sbvs.bat
cd ..
cd ..
cd proofs
..\..\..\..\dist\build\sawScript\sawScript -c ..\..\build -j..\..\..\..\jdk1.6\classes.jar +RTS -K64M -RTS toplevel.saw
cd ..
