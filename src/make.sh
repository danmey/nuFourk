#!/bin/bash

ocamllex -ml lexer.mll
output=`ocamlfind ocamlc -package batteries -linkpkg unification.ml lexer.ml code.ml engine.ml repl.ml -o repl  2>&1 | grep -A 100 -B 10 Error:`
if [[ -z $output ]]; then
    exit 0
else
    echo "$output"
    exit -1
fi
