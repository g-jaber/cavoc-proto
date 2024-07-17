(* To be tested with
./_build/default/bin/explore.exe test/no_visible.ml test/no_visible.mli
*)

let r = ref 0

let g f = f (fun () -> r := !r +1); let n = ! r in f (fun x -> x); assert (n = !r)