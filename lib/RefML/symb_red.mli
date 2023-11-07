type symbconf =
  Syntax.exprML
  * Syntax.val_env
  * Logic.symbheap
  * Logic.symbheap
  * Type_ctx.var_ctx
  * Logic.arith_pred list

val symbred : Logic.symbheap -> Syntax.exprML -> symbconf list * bool
val symbred_trans : symbconf -> symbconf list
val compute_nf : Syntax.exprML -> symbconf list
