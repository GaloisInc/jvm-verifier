#!/bin/sh

# Script to update to JRE 1.6 on Mac OS X 10.5.8.
# For instructions, see:
#  http://www.piranhamethod.com/2010/04/installing-jre-1-6-on-your-mac-os-x-leopard-10-5-8-or-later/

cd /System/Library/Frameworks/JavaVM.framework/Versions

CURJRE="`readlink Current`"
echo Current JRE version: $CURJRE

if [ "$1" == "" ]; then
echo Installed versions:
ls
exit
fi

VERFOUND=`ls | grep $1 | head -n 1`

if [ "$VERFOUND" != "$1" ]; then
BASE="`basename $0`"
echo Error: Could not change JRE-- version $1 not installed!
echo Run $BASE without arguments to see a list of installed versions.
exit 127
fi

echo You must now enter your Mac OS X password to change the JRE.
sudo ln -fhsv $1 Current

