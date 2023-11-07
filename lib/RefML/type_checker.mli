exception TypingError of string

val infer_type :
  Type_ctx.var_ctx ->
  Type_ctx.loc_ctx ->
  Type_ctx.name_ctx ->
  Types.type_subst ->
  Syntax.exprML ->
  Types.typeML * Type_ctx.var_ctx * Types.type_subst

val typing_full : Types.type_subst -> Syntax.exprML -> Types.typeML
