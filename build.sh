#!/bin/bash

ocamlc -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt pa_calculator.ml
