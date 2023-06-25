type heap
val emptyheap : heap
val allocate : heap -> Syntax.exprML -> Syntax.loc * heap
val modify : heap -> Syntax.loc -> Syntax.exprML -> heap
val access : heap -> Syntax.loc -> Syntax.exprML option
val string_of_heap : heap -> string
