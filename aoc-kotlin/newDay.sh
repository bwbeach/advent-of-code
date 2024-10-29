#!/bin/bash

if [[ -z "$1" ]]
then
    echo "Usage: newDay.sh <projName>"
    exit 1
fi

./gradlew :newDay:build

mkdir tmp
cd tmp
tar xvf ../newDay/build/distributions/newDay.tar
cd ..

tmp/newDay/bin/newDay $1

rm -rf tmp

git add $1
git add settings.gradle.kts

