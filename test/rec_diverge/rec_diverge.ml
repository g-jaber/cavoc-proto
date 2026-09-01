(* To be tested with
dune exec bin/explore.exe test/rec_diverge.ml test/rec_diverge.mli
*)
let rec f x = f x