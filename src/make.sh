#!/bin/bash

ocamllex -ml lexer.mll
ocamlfind ocamlc -package batteries -linkpkg lexer.ml engine.ml repl.ml -o repl

