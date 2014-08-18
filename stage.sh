#!/bin/sh

TARGET=jss-0.4

NM=`uname`

mkdir -p ${TARGET}/bin
mkdir -p ${TARGET}/doc
mkdir -p ${TARGET}/tutorial

if [ "${OS}" == "Windows_NT" ]; then
  EXEDIR=windows
elif [ "${NM}" == "Darwin" ]; then
  EXEDIR=macosx  
else
  EXEDIR=linux
fi

echo Staging ...

cp RELEASE_README                              ${TARGET}/README
cp ../../abcBridge/abc-build/copyright.txt     ${TARGET}/ABC_LICENSE
cp -R doc/japi                                 ${TARGET}/doc
cp doc/jss-usage.txt                           ${TARGET}/doc
cp doc/jss.1                                   ${TARGET}/doc
cp doc/japi-tutorial/jss-tutorial.*            ${TARGET}/tutorial
cp doc/japi-tutorial/code/*.{class,cry,java}   ${TARGET}/tutorial
cp -R doc/japi-tutorial/images                 ${TARGET}/tutorial
cp dist/build/jss/jss                          ${TARGET}/bin
cp support/galois.jar                          ${TARGET}/bin

if [ "${OS}" == "Windows_NT" ]; then
  zip -r ${TARGET}-${EXEDIR}.zip ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.zip"
else
  tar cvfz ${TARGET}-${EXEDIR}.tar.gz ${TARGET}
  echo "Release package is ${TARGET}-${EXEDIR}.tar.gz"
fi
