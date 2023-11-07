(* This file provides a definitional interpreter for RefML *)

type opconf = Syntax.exprML * Syntax.val_env * Heap.heap
val string_of_opconf : opconf -> string
type mem_state = opconf list
type 'a m = mem_state -> 'a list * mem_state

val lookup : opconf -> bool m
val add : opconf -> unit m
val interpreter : (opconf -> opconf m) -> opconf -> opconf m
val compute_nf_monad : opconf -> opconf m
val compute_nf : opconf -> opconf option
val compute_valenv : (Syntax.id*Syntax.exprML) list -> (Syntax.val_env*Heap.heap)
