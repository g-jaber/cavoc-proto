type heap = (Syntax.loc, Syntax.value) Util.Pmap.pmap [@@deriving to_yojson]

val pp_heap : Format.formatter -> heap -> unit
val string_of_heap : heap -> string

val emptyheap : heap
val allocate : heap -> Syntax.value -> Syntax.loc * heap
val modify : heap -> Syntax.loc -> Syntax.value -> heap
val update : heap -> heap -> heap
val lookup : heap -> Syntax.loc -> Syntax.value option
val loc_ctx_of_heap : heap -> Type_ctx.loc_ctx
val generate_heaps : Type_ctx.loc_ctx -> heap list
val restrict : Type_ctx.loc_ctx -> heap -> heap