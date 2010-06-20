#!/bin/bash

ocamllex -ml parser.mll
ocamlc parser.ml engine.ml repl.ml -o repl

