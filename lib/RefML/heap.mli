type heap = (Syntax.loc, Syntax.term) Util.Pmap.pmap

val emptyheap : heap
val allocate : heap -> Syntax.term -> Syntax.loc * heap
val modify : heap -> Syntax.loc -> Syntax.term -> heap
val access : heap -> Syntax.loc -> Syntax.term option
val string_of_heap : heap -> string
val loc_ctx_of_heap : heap -> Type_ctx.loc_ctx
val generate_heaps : Type_ctx.loc_ctx -> heap list
