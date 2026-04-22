# cavoc-proto

CAVOC is a framework for building interactive models of programming languages based on operational game semantics.
It provides the possibility to explore dynamically the semantics of a program module: you take the role of a client of the module, 
interacting with it by choosing which input action to perform.

Two executables are provided:
* ``explore-cli``, that is used via a command-line interface
* ``explore-web``, that provided a web interface

Currently, the only programming language supported is a fragment of OCaml with:
  * higher-order functions
  * recursive function-definitions and while loops
  * Hindley-Milner polymorphic type system with value restriction
  * type abstraction via signature (.ml/.mli organisation)
  * integer, booleans, product and sum data-types
  * dynamically generated mutable references
  * assertions that trigger uncatchable errors
  * exception and try-with

## Installation and use

You can build the project using ``dune build``.
Its dependencies are ``lwt``, ``js_of_ocaml``, and ``yojson``.

## explore-web

To run it, you first need to run a simple web server via ``dune exec ./bin/server.exe``.
Then you can use your web browser to go to:
- http://localhost:8000/front/indextuto.html for a tutorial
- http://localhost:8000/front/index.html for free use

## explore-cli

To run it, use ``dune exec ./bin/explore.exe example.ml example.mli``: it takes a module ``example.ml`` and its signature ``example.mli``, 
and provides


The directory ``test/`` contains multiple examples of modules and programs on which ``explore`` can be tested.


You can also pass the following options to ``explore``:
  * -generate-tree, if you want an exhaustive representation of the interaction rather than an interactive one. 
    To keep the exploration finite, only the normal-form tree is computed. It is then printed using the dot format.
  * -compare, if you want to check if the implementations of two modules are observationally equivalent.
    It computes the synchronization product of the normal-form trees of the two modules.
  * -program, if you want to explore the behavior of a program rather than a module. With this option, you do not have to provide a .mli file.
  * -no-wb, if you want to allow Opponent to not respect the well-bracketing enforcement of the interaction.
    This corresponds to allowing the client of the module to use a control operator like call/cc.
  * -vis, if you want to enable the visibility enforcement of the interaction.
    This corresponds to forbidding the client to use higher-order references to store the functions provided by the module during the interaction.
  * -no-cps, Use a representation of actions as calls and returns rather than in cps style. 
    This is incompatible with the -vis option.

