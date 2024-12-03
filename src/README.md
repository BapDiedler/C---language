
* Compiler from C-- to LC3

This is a partial implementation in OCaml of a compiler from a simple version of C into the LC3 assembly language.

** Files

*** Makefile (MAY CHANGE)

Makefile used to
- call dune to compile OCaml code

> make 

- run test

> make test

- clean the directory

> make clean

May be changed to add more tests.


*** cast.mli, cast.ml (DO NOT CHANGE)

OCaml module including the definition of AST for C-- and functions required to build this AST.

*** clexer.mll (DO NOT CHANGE)

OCamllex module including the definition of lexemes and used to generate (see dune file) the Ocaml code for the lexical analysis.

*** compile.ml (TO WRITE)

Module implementing the synthesis of LC3 code.

*** cparser.mly (DO NOT CHANGE)

Menhir module including the definition of the syntax for C-- and used to generate (see dune file) the OCaml code for the syntactical analysis.

*** types.mli (MAY CHANGE)

All the types allowed and needed for type-checking.

*** ctyping.ml (TO WRITE)

Module implementing the typing rules.

*** dune, dune-project (MAY CHANGE)

Files read by dune tool to automatically compile files of an OCaml project.

May be changed to add modules without implementation, if needed.

If possible to execute all tests with make test.

*** main.ml (MAY CHANGE)

Contains the main function calling the functions coding the different steps of the compiler.

May be modified to include more compiler steps.

*** pretty.ml (MAY CHANGE)

Utility module to print AST in a format that may be visualized nicely, i.e., DOT format of GRAPHVIZ tool.

*** test.c (DO NOT CHANGE)

A first example of test file that now passes lexical and syntactic analysis.
See directory ../tst for mode tests.

*** usage.ml (MAY CHANGE)

Module for options of the compiler.

May change if more options added.

