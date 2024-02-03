#!/bin/bash

# i.e make clean
scala-cli clean . && rm -f wacc-compiler -o wacc-compiler

# i.e make
scala-cli --power package . -o wacc-compiler --server=false --jvm system --force