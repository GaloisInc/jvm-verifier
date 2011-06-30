#!/bin/sh

TARGET=beta-rc3

NM=`uname`

mkdir -p ${TARGET}/bin
mkdir -p ${TARGET}/doc
mkdir -p ${TARGET}/tutorial
mkdir -p ${TARGET}/examples/ecc

if [ "${OS}" == "Windows_NT" ]; then
  EXEDIR=windows
  cp cabal-dev/bin/abc.dll ${TARGET}/bin
elif [ "${NM}" == "Darwin" ]; then
  EXEDIR=macosx  
else
  EXEDIR=linux
fi

echo Staging ...

cp RELEASE_README                              ${TARGET}/README
cp beta-rc1/ABC_LICENSE                        ${TARGET}
cp -R doc/japi                                 ${TARGET}/doc
cp doc/jss-usage.txt                           ${TARGET}/doc
cp doc/japi-tutorial/jss-tutorial.pdf          ${TARGET}/tutorial
cp doc/japi-tutorial/code/*.{class,cry,java}   ${TARGET}/tutorial
cp doc/sawScriptTutorial/sawScriptTutorial.pdf ${TARGET}/tutorial
cp dist/build/jss/jss                          ${TARGET}/bin
cp dist/build/sawScript/sawScript              ${TARGET}/bin
cp support/galois.jar                          ${TARGET}/bin
cp -R Examples/ECC/                            ${TARGET}/examples/ecc

if [ "${OS}" == "Windows_NT" ]; then
  zip -r ${TARGET}-${EXEDIR}.zip ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.zip"
else
  tar cvfz ${TARGET}-${EXEDIR}.tar.gz ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.tar.gz"
fi
