#!/bin/bash

echo "digraph {"

cat $1 | \
    sed -e 's/^[%&]//' | \
    sed -e 's/^/    /' | \
    sed -e 's/-> /-> { /' | \
    sed -e 's/,//g' | \
    sed -e 's/$/ }/'

cat $1 | \
    egrep '^[%]' | \
    sed -e 's/^[%&]//' | \
    sed -e 's/ .*//' | \
    sed -e 's/^/    /' | \
    sed -e 's/$/ [color = blue]/'

cat $1 | \
    egrep '^[&]' | \
    sed -e 's/^[%&]//' | \
    sed -e 's/ .*//' | \
    sed -e 's/^/    /' | \
    sed -e 's/$/ [color = red]/'

echo "}"
