#!/bin/bash

cd "${0%/*}";

IFS=$'\n' args=$1;

k=1;
for i in $args; do
  IFS=' ' j=$i;
  echo -ne "$k -- $j: ";
  ./main.native -k sni $j;
  k=$((k+1));
done;
