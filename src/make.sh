#!/bin/bash

ocamllex -ml lexer.mll
output=`ocamlfind ocamlc -package batteries -linkpkg unify.ml lexer.ml code.ml type.ml engine.ml repl.ml -o repl  2>&1 | grep -A 100 -B 2 Error:`
if [[ -z $output ]]; then
    exit 0
else
    echo "$output"
    exit -1
fi
