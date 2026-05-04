CAVOC is parametric with respect to the programming language (called machine language in this setting) used to write the code of modules and clients.
To create a new machine language to use CAVOC on,
you have to provide an instance of the signature Lib.Lang.LANG_WITH_INIT.

CAVOC provides a functor Lib.Lang.Make to build such an instance from a lower representation, namely an instance of Lib.Lang.Language.WITHAVAL_NEG. 

This signature Lib.Lang.Language.WITHAVAL_NEG enforces that evaluation contexts (i.e. evaluation stacks) are part of values of the language.
We provide another signature, Lib.Lang.Language.WITHAVAL_INOUT, for languages for which evaluation stacks are distinct syntactic entities.
We then have a functor Lib.Lang.Cps.MakeComp to transform automatically an instance of WITHAVAL_INOUT into an instance of WITHAVAL_NEG. This functor performs an on-the-fly cps translation of the normal-forms computed by the evaluator of the language.


The lib/ directory contains the various subparts of the prototype:
- util/ contains some basic utility modules for printing, monads, partial maps and span. They should eventually be replaced by proper implementation found in existing libraries.
- lang/ contains the interfaces used to represent the programming languages:
  * language.ml contains the various signatures to represent the core of the programming language,
  including the notion of abstracted values;
  * nf.ml contains the definition of normal forms, that is parametrized w.r.t. the type of values, evaluation contexts, function names and continuation names.
  * interactive.ml contains the signature used to represent the focusing process that decomposes normal forms using the abstraction process of values.
  * cps.ml contains the interactive continuation-passing-style transformation of normal forms and top-level terms of a language that embeds evaluation contexts into values.
- lts/ contains the interfaces used to represent the LTSs used to represent the interaction.
  * The signature MOVES is given in moves.ml, and represents the basic notions of moves, that carry a direction and a mix of kind and data (called move). We provide a functor Make (IntLang : Lang.Interactive.LANG) that defined a module Moves using
  abstracted normal forms as move.
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