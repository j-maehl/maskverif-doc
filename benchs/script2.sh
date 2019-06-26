#! /bin/bash

change() {
  local filename=$1
  local suffix=$2
  local args

  shift 2; args=$*

  local basename=$(basename $filename)
  local dirname=$(dirname $filename)

  sed "s/SNI *\(.*\)$/$args \1/" "$filename" \
    > "$dirname/${basename/sni-/$suffix-}"
}

for i in $*; do 
  change "$i" ng-sni noglitch SNI
  change "$i" ni     NI
  change "$i" ng-ni  noglitch NI
  change "$i" pr     Probing
  change "$i" ng-pr  noglitch Probing
done

## find . -name '*.mv' -exec ./script2.sh '{}' \;
