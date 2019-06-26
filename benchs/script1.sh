#! /bin/bash
# maskverif should be in your path
for i in $*; do
  echo $i;
  (time maskverif) < $i > ${i/.mv/.log} 2>&1;
done
