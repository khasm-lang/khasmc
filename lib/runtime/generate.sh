#!/usr/bin/env bash
pwd
echo "let runtime_c = {onetwothreefour|" > runtime_lib.ml
cpp all.c >> runtime_lib.ml
echo "|onetwothreefour}" >> runtime_lib.ml
