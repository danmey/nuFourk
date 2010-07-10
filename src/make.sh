#!/bin/bash

ocamllex -ml parser.mll
ocamlfind ocamlc -package batteries -linkpkg parser.ml engine.ml repl.ml -o repl

