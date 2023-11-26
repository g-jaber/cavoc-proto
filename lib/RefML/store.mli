type store  = Syntax.val_env * Heap.heap

val string_of_store : store -> string
val empty_store : store
val loc_lookup :  store -> Syntax.loc -> Syntax.value option
val var_lookup :  store -> Syntax.id -> Syntax.value option
val loc_allocate : store -> Syntax.value -> (Syntax.loc*store)
val loc_modify : store ->  Syntax.loc -> Syntax.value -> store
val var_add : store -> (Syntax.id*Syntax.value) -> store

type store_ctx = Type_ctx.loc_ctx

val empty_store_ctx : store_ctx
val string_of_store_ctx : store_ctx -> string
val concat_store_ctx : store_ctx -> store_ctx -> store_ctx
val infer_type_store : store -> store_ctx

(* update_store (env,h) (env',h') is equal to (env,h[h']) *)
val update_store : store -> store -> store
val restrict : store_ctx -> store -> store