(* A program whose free name ctx is declared in test/wbsc_context/wbsc_context.mli. *)

let x = ref 0 in (ctx (fun f -> x := 0; f(); x:=1; f (); !x))+1