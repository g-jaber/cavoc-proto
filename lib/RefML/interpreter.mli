(* This file provides a definitional interpreter for RefML *)

type memory = Syntax.val_env * Heap.heap
type opconf = Syntax.term * memory

val normalize_opconf : opconf -> opconf option
val normalize_term_env : Declaration.comp_env -> memory
