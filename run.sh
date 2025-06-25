#!/bin/bash
make clean
make

if [ "$#" -lt 1 ]; then
    echo "> only compile."
    exit 1
fi

dune exec ./maskverif.exe < $1
