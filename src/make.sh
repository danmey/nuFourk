#!/bin/bash

ocamllex -ml lexer.mll
output=`ocamlfind ocamlc -package batteries -linkpkg unification.ml lexer.ml engine.ml repl.ml -o repl 3&>1`

if [[ -z $output ]]; then
    exit 0
else
    echo "$output"
    exit -1
fi


