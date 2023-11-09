type interactive_val
type glue_val

val embed_val : Syntax.valML -> interactive_val

type interactive_env

type kindTerm =
  | Extern of (Syntax.name * glue_val * Heap.heap)
  | IsRecCall of
      (Syntax.id
      * Syntax.exprML
      * Syntax.exprML
      * Syntax.eval_context
      * Heap.heap)
  | Diverge

val empty_ienv : interactive_env
val singleton_ienv : Syntax.name * interactive_val -> interactive_env
val list_to_ienv : (Syntax.name * interactive_val) list -> interactive_env
val lookup_ienv : Syntax.name -> interactive_env -> interactive_val option
val concat_ienv : interactive_env -> interactive_env -> interactive_env
val string_of_interactive_env : interactive_env -> string
val decompose_nf : Interpreter.opconf -> Syntax.name * glue_val
val decompose_nf_option : Interpreter.opconf option -> kindTerm
val val_composition : interactive_val -> Syntax.valML -> Syntax.exprML

val abstract_glue_val :
  glue_val -> Types.typeML -> Nup.nup * interactive_env * Type_ctx.name_ctx

val subst_names_of_nup : interactive_env -> Nup.nup -> Syntax.valML
