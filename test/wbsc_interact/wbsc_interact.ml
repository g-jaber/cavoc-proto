(* To be tested with the following command:

dune exec bin/compose.exe test/wbsc_interact.ml test/wbsc_context.ml test/wbsc_context.mli
*)

let x = ref 0 in (_ctx (fun f -> x := 0; f(); x:=1; f (); !x))+1