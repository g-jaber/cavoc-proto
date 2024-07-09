(* To be tested with
dune exec explore.exe test/private_counter.ml test/private_counter.mli
*)

type t = int

let c = ref 0
let get ()= c:=!c+1;!c 
let check x = assert ((x > 0) && (x <= !c))