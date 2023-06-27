open Syntax

type nup = exprML
val fresh_fname : unit -> id
val fresh_cname : unit -> id

type name_ctx = (id,Types.typeML) Pmap.pmap

val generate_nup : Types.typeML -> (nup*name_ctx) list



type kindTerm =
    IsVal of (id*exprML)
  | IsCallExtern of (id*exprML*eval_context)
  | IsRecCall of (id*exprML*exprML*eval_context)

val decompose_nf : full_expr -> kindTerm

type interactive_env = (id, Syntax.exprML) Pmap.pmap

val abstract_val : exprML -> Types.typeML -> nup * interactive_env * name_ctx


val string_of_interactive_env : interactive_env -> string

type action =
  | Op
  | RecCall of id 
  | PQ of id*nup*id
  | PA of id*nup
  | OQ of id*nup*id
  | OA of id*nup