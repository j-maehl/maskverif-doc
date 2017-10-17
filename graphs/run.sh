#! /bin/sh

./genGraph.native `echo $1 | perl -pe 's/([^0-9])/" "/ge'`

