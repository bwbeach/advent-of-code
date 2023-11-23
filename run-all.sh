#! /bin/bash

for d in libs/*
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
    cabal repl --with-ghc=doctest
    if [[ $? -eq 0 ]]
    then
       echo DOCTEST SUCCESS
    else
       echo DOCTEST FAIL
       exit 1
    fi
    cabal test
    if [[ $? -eq 0 ]]
    then
        echo SUCCESS
    else
        echo FAILED
        exit 1
    fi
    popd
done

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
    cabal repl --with-ghc=doctest
    if [[ $? -eq 0 ]]
    then
       echo DOCTEST SUCCESS
    else
       echo DOCTEST FAIL
       exit 1
    fi
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
