(* To be tested with
   ./_build/default/bin/explore.exe test/wbsc.ml test/wbsc.mli 
   
in order to study non well-bracketed interaction. *)

let x = ref 0
let g f = x := 0; f(); x:=1; f (); assert (!x = 1)