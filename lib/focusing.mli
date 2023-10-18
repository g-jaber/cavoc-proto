type nup
val generate_nup : Types.typeML -> (nup*Syntax.name_ctx) list
val string_of_nup : nup -> string

type kindTerm =
    Extern of (Syntax.name*Syntax.exprML*Heap.heap)
  | IsRecCall of (Syntax.id*Syntax.exprML*Syntax.exprML*Syntax.eval_context*Heap.heap)
  | Diverge

type interactive_env
val empty_ienv : interactive_env
val singleton_ienv : (Syntax.name*Syntax.exprML) -> interactive_env
val list_to_ienv : (Syntax.name*Syntax.exprML) list -> interactive_env
val lookup_ienv : Syntax.name -> interactive_env -> Syntax.exprML option
val concat_ienv : interactive_env -> interactive_env -> interactive_env
val string_of_interactive_env : interactive_env -> string

val decompose_nf : Interpreter.opconf -> (Syntax.name*Syntax.exprML)
val decompose_nf_option : Interpreter.opconf option -> kindTerm

val val_composition : Syntax.exprML -> nup -> Syntax.exprML

val abstract_val : Syntax.valML -> Types.typeML -> nup * interactive_env * Syntax.name_ctx



val unify_nup :
  Syntax.name Namespan.namespan ->
  nup -> nup -> Syntax.name Namespan.namespan option
