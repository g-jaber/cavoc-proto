(** This module provides a definitional interpreter for RefML *)

type opconf = Syntax.term * Store.store

val normalize_opconf : opconf -> opconf list
(* [val_env] is the initial value environment the declarations are evaluated in,
   so
   that identifiers bound there resolve during evaluation (Var is resolved by
   lookup, not by substitution). Used to make imported names visible. *)
val normalize_term_env :
  ?val_env:Syntax.val_env ->
  Type_ctx.cons_ctx -> Declaration.comp_env -> Store.store
