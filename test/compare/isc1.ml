(* to be tested with
   ./_build/default/bin/compare.exe test/compare/isc1.ml test/compare/isc2.ml *)
let x = ref 0 in
fun f -> x:=!x+1;f();!x