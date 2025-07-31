exception TypingError of string

val typing_expr : Type_ctx.type_ctx -> Syntax.term -> Type_ctx.type_ctx * Types.typ

val checking_expr : Type_ctx.type_ctx -> Syntax.term ->  Types.typ -> Type_ctx.type_ctx * Types.typ