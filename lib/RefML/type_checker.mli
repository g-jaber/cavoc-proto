exception TypingError of string

val infer_type :
  Type_ctx.type_ctx -> Types.type_subst -> Syntax.term -> Types.typ * Types.type_subst

val infer_gen_type :
  Type_ctx.type_ctx -> Types.type_subst ->  Syntax.term -> Types.typ * Types.type_subst

val typing_full : Types.type_subst -> Syntax.term -> Types.typ
