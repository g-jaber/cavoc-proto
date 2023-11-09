exception TypingError of string

val infer_type :
  Type_ctx.type_ctx -> Syntax.exprML -> Types.typeML * Type_ctx.type_ctx

val infer_gen_type :
  Type_ctx.type_ctx -> Syntax.exprML -> Types.typeML * Type_ctx.type_ctx

val typing_full : Types.type_subst -> Syntax.exprML -> Types.typeML
