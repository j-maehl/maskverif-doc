#! /bin/bash
# maskverif should be in your path

for i in $(find . -name '*.mv'); do
  echo $i;
  (time maskverif) < $i > ${i/.mv/.log} 2>&1;
done
