(* To be tested with
dune exec bin/explore.exe test/isc.ml test/isc.mli
*)
let x = ref 0
let g f = x:=1;f();assert (!x = 1)