(* To be tested with
./_build/default/bin/explore.exe test/higher_order.ml test/higher_order.mli
*)
let x = ref 0
let bli f = f (fun () -> x:=!x+1);!x