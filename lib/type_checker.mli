exception TypingError of string

val infer_type :
  Syntax.var_ctx ->
  Syntax.loc_ctx ->
  Types.type_subst ->
  Syntax.exprML ->
  Types.typeML * Syntax.var_ctx * Types.type_subst

val typing_full : Syntax.exprML -> Types.typeML
