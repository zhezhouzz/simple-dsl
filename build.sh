#!/bin/bash

ocmalc -c Ast.ml
ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
mkdir -p build
cd build
echo ${OCAML_CMAKE_PLUGIN}
cmake .. -DCMAKE_MODULE_PATH:STRING=${OCAML_CMAKE_PLUGIN}
make
cd -
