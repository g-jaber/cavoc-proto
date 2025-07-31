(* dune exec bin/explore.exe -- -program test/let_polymorphism.ml *)

let f x = x in (f true, f 5)