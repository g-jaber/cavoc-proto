# cavoc-proto

You can build the project using ``dune build``.

The prototype is currently formed by an executable ``explore``.
To run it, use ``dune exec explore example.ml example.mli``: it takes a module ``example.ml`` and its signature ``example.mli``, 
and provides a way to play the environment role (a.k.a. Opponent) in the interaction, by choosing which input action to perform.
In that setting, the environment can be seen as a client of the module.

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

The directory ``test/`` contains multiple examples of modules and programs on which ``explore`` can be tested.

The prototype currently works on a fragment of OCaml with:
  * higher-order functions
  * recursive function-definitions and while loops
  * Hindley-Milner polymorphic type system with value restriction
  * type abstraction via signature (.ml/.mli organisation)
  * integer, booleans, product and sum data-types
  * dynamically generated mutable references
  * assertions that trigger uncatchable errors
  * exception and try-with

There is also another, experimental (a.k.a. mostly broken) executable ``compose``, that takes a module and its signature, and a client, 
and compose them, computing their interaction.

The lib/ directory contains the various subparts of the prototype:
- util/ contains some basic utility modules for printing, monads, partial maps and span. They should eventually be replaced by proper implementation found in existing libraries.
- lang/ contains the interfaces used to represent the programming languages:
  * language.ml contains the various signatures to represent the core of the programming language,
  including the notion of abstracted values;
  * nf.ml contains the definition of normal forms, that is parametrized w.r.t. the type of values, evaluation contexts, function names and continuation names.
  * interactive.ml contains the signature used to represent the focusing process that decomposes normal forms using the abstraction process of values.
  * cps.ml contains the interactive continuation-passing-style transformation of normal forms and top-level terms of a language that embeds evaluation contexts into values.
- lts/ contains the interfaces used to represent the LTSs used to represent the interaction.
  * The signature MOVES is given in moves.ml, and represents the basic notions of moves, that carry a direction and a mix of kind and data (called kdata). We provide a functor Make (IntLang : Lang.Interactive.LANG) that defined a module Moves using
  abstracted normal forms as kdata.
  * The signature ACTIONS is given in actions.ml, and embeds moves into actions.
  * The signature INT in interactive.ml provides the functions to generate moves from name contexts and normal forms,
  and to build terms from moves and interactive environments.
  * a functor Make (IntLang : Lang.Interactive.LANG) : INT  is provided in the file interactive.ml to build a module of signature INT.
  * The signature INT_LTS in bipartite.ml is the  basic notion of bipartite LTS
  * We provide a generic way to compute the product of two bipartite LTS via the functor Make of the file product_lts.ml
- ogs/ contains:
  * the definition of the OGS LTS in ogslts.ml, defined by a functor over Interactive.INT signature 
  * including the well-bracketed part.
- RefML/ contains the definition of the main instance of a programming language:
  * refml.ml contains the instantiation of the signature Interactive.WITHAVAL
  * interpreter.ml contains the monadic definitional interpreter.