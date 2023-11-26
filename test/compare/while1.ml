(* to be tested with
   ./_build/default/bin/compare.exe test/compare/while1.ml test/compare/while2.ml *)
let f g = while true do g () in f