#!/usr/bin/env bash
pwd
echo "let runtime_c = {onetwothreefour|" > runtime_lib.ml
xargs cat < ../../../lib/runtime/all.c >> runtime_lib.ml
echo "|onetwothreefour}" >> runtime_lib.ml
grep -v '^#include \"' runtime_lib.ml > runtime_lib_tmp.ml
mv runtime_lib_tmp.ml runtime_lib.ml

