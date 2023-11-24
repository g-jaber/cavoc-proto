type symbconf =
  Syntax.term
  * Syntax.val_env
  * Logic.symbheap
  * Logic.symbheap
  * Type_ctx.var_ctx
  * Logic.arith_pred list

val symbred : Logic.symbheap -> Syntax.term -> symbconf list * bool
val symbred_trans : symbconf -> symbconf list
val compute_nf : Syntax.term -> symbconf list
