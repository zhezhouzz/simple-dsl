cmake_minimum_required(VERSION 2.6)

project(simple-dsl NONE)


# set(CMAKE_MODULE_PATH "")

find_package(OCaml REQUIRED)

include(UseOCaml)

add_ocaml_executable(calc.out SOURCES
    lexer.ml
    parser.mli
    parser.ml
    calc.ml
    ast.ml)