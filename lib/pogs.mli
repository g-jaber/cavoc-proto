type arith_ctx = Logic.arith_pred list
type id_conf
type nup = Syntax.exprML
val fresh_fname : unit -> Syntax.id
val fresh_cname : unit -> Syntax.id
val abstract_val : Syntax.exprML -> nup * (Syntax.id, nup) Pmap.pmap

type kindTerm =
    IsVal of (Syntax.id*Syntax.exprML)
  | IsCallExtern of (Syntax.id*Syntax.exprML*Syntax.eval_context)
  | IsRecCall of (Syntax.id*Syntax.exprML*Syntax.exprML*Syntax.eval_context)

val decompose_nf : Syntax.full_expr -> kindTerm
val fresh_id_sequent : unit -> id_conf
type interactive_env = (Syntax.id, nup) Pmap.pmap
type move = Syntax.id * nup
type active_conf = {
  id : id_conf;
  ground_var_ctx : Syntax.var_ctx;
  alpha : (Syntax.var_ctx, bool) Pmap.pmap;
  arith_ctx : arith_ctx;
  term : Syntax.exprML;
}
type passive_conf = {
  id : id_conf;
  ground_var_ctx : Syntax.var_ctx;
  alpha : (Syntax.var_ctx, bool) Pmap.pmap;
  arith_ctx : arith_ctx;
  ienv : interactive_env;
}
