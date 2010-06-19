#!/bin/bash

ocamllex -ml parser.mll
ocamlc parser.ml repl.ml -o repl

