type heap = (Syntax.loc, Syntax.exprML) Util.Pmap.pmap

val emptyheap : heap
val allocate : heap -> Syntax.exprML -> Syntax.loc * heap
val modify : heap -> Syntax.loc -> Syntax.exprML -> heap
val access : heap -> Syntax.loc -> Syntax.exprML option
val string_of_heap : heap -> string
val loc_ctx_of_heap : heap -> Type_ctx.loc_ctx
val generate_heaps : Type_ctx.loc_ctx -> heap list
