exception TypingError of string

val infer_type :
  Syntax.var_ctx ->
  Syntax.loc_ctx ->
  Syntax.name_ctx ->
  Types.type_subst ->
  Syntax.exprML ->
  Types.typeML * Syntax.var_ctx * Types.type_subst

val typing_full : Types.type_subst -> Syntax.exprML -> Types.typeML
