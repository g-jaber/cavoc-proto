(* to be tested with
   dune exec bin/explore_cli.exe -- -compare -program test/compare/while1.ml test/compare/while2.ml *)
let f g = while true do g () done in f