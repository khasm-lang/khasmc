#!/bin/bash

echo "let runtime_c = {|" > ./../../../lib/runtime_lib.ml
for file in ./../../../lib/runtime/*.h
    	    do
	    echo "$file" >> ./../../../lib/runtime_lib.ml
done

for file in ./../../../lib/runtime/*.c
    	    do
    	    echo "$file" >> ./../../../lib/runtime_lib.ml
done
sed -i '/^#include/d' ./../../../lib/runtime_lib.ml
echo "|}" >> ./../../../lib/runtime_lib.ml
