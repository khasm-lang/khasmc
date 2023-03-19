#!/bin/bash

echo "let runtime_c = {khasmkhasmkhasm|" > ./../lib/runtime_lib.ml
f="./../lib/runtime_lib.ml"
\cat "./../../../lib/runtime/types.h" >> "$f"
\cat "./../../../lib/runtime/khagm_obj.h" >> "$f"
\cat "./../../../lib/runtime/khagm_alloc.h" >> "$f"
\cat "./../../../lib/runtime/khagm_eval.h" >> "$f"
\cat "./../../../lib/runtime/create.h" >> "$f"
\cat "./../../../lib/runtime/dispatch.h" >> "$f"
\cat "./../../../lib/runtime/err.h" >> "$f"

for file in ./../../../lib/runtime/*.c
    	    do
    	    \cat "$file" >> ./../lib/runtime_lib.ml
done
sed -i '/^#include/d' ./../lib/runtime_lib.ml
echo "|khasmkhasmkhasm}" >> ./../lib/runtime_lib.ml
