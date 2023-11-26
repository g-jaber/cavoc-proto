type memory  = Syntax.val_env * Heap.heap

val string_of_memory : memory -> string
val empty_memory : memory
val loc_lookup :  memory -> Syntax.loc -> Syntax.value option
val var_lookup :  memory -> Syntax.id -> Syntax.value option
val loc_allocate : memory -> Syntax.value -> (Syntax.loc*memory)
val loc_modify : memory ->  Syntax.loc -> Syntax.value -> memory
val var_add : memory -> (Syntax.id*Syntax.value) -> memory

type memory_type_ctx = Type_ctx.loc_ctx

val empty_memory_type_ctx : memory_type_ctx
val string_of_memory_type_ctx : memory_type_ctx -> string
val concat_memory_ctx : memory_type_ctx -> memory_type_ctx -> memory_type_ctx
val infer_type_memory : memory -> memory_type_ctx

(* update_memory (env,h) (env',h') is equal to (env,h[h']) *)
val update_memory : memory -> memory -> memory