type store = Syntax.val_env * Heap.heap * Type_ctx.cons_ctx

val string_of_store : store -> string
val pp_store : Format.formatter -> store -> unit
val empty_store : store
val loc_lookup :  store -> Syntax.loc -> Syntax.value option
val var_lookup :  store -> Syntax.id -> Syntax.value option
val cons_lookup :  store -> Syntax.id -> Types.typ option
val loc_allocate : store -> Syntax.value -> (Syntax.loc*store)
val loc_modify : store ->  Syntax.loc -> Syntax.value -> store
val var_add : store -> (Syntax.id*Syntax.value) -> store
val cons_add : store -> (Syntax.constructor*Types.typ) -> store

val embed_cons_ctx : Type_ctx.cons_ctx -> store

type store_ctx = Type_ctx.loc_ctx * Type_ctx.cons_ctx

val empty_store_ctx : store_ctx
val string_of_store_ctx : store_ctx -> string
val pp_store_ctx : Format.formatter -> store_ctx -> unit
val concat_store_ctx : store_ctx -> store_ctx -> store_ctx
val infer_type_store : store -> store_ctx

(* update_store (env,h) (env',h') is equal to (env,h[h']) *)
val update_store : store -> store -> store
val restrict : store_ctx -> store -> store

type label = Syntax.label

val restrict_ctx : store_ctx -> label list -> store_ctx