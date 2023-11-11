#! /bin/bash

for d in 2*/day*
do
    echo
    echo ======================================== 
    echo =
    echo = $d
    echo =
    echo ======================================== 
    echo
    echo $d
    pushd $d
    cabal run
    if [[ $? -eq 0 ]]
    then
        echo SUCCESS
    else
        echo FAILED
        exit 1
    fi
    popd
done
