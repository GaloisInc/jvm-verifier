rmdir /s /q build
mkdir build
javac -g -cp src;..\..\support src\com\galois\ecc\*.java -d build
javac -g -cp build;tests tests\com\galois\ecc\*.java -d build
cd saw
cd proofs
..\..\..\..\bin\sawScript.exe -c ..\..\build -j"C:\Program Files\Java\jre7\lib\rt.jar" +RTS -K64M -RTS toplevel.saw
cd ..\..
