(* This file provides a definitional interpreter for RefML *)

type opconf = Syntax.term * Store.store

val normalize_opconf : opconf -> opconf option
val normalize_term_env : Declaration.comp_env -> Store.store
