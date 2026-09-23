(* A configuration of the symbolic evaluation: the term, the value environment,
   the required heap (the ground locations accessed before any write, holding
   their symbolic variables), the resulting heap (the locations accessed or allocated;
   absence means untouched), the declarations and the constraints. *)
type symbconf =
  Syntax.term
  * Syntax.val_env
  * Heap.heap
  * Heap.heap
  * Symbolic.symbolic_ctx
  * Logic.arith_pred list

(* A step bound reached is incomplete evaluation, never divergence. *)
type run_result =
  | Normal_form of symbconf
  | Divergence of symbconf
  | Incomplete of symbconf

(* One step under the ground heap context, false on a normal form. *)
val symbred :
  Type_ctx.loc_ctx -> Heap.heap -> Syntax.term -> symbconf list * bool

(* Every feasible branch: a branch is dropped when the solver answers Unsat. *)
val symbred_trans :
  ?bound:int ->
  check_sat:(Symbolic.symbolic_ctx -> Logic.arith_ctx -> Arith_solver.answer) ->
  Type_ctx.loc_ctx ->
  symbconf ->
  run_result list

val compute_nf :
  ?bound:int ->
  check_sat:(Symbolic.symbolic_ctx -> Logic.arith_ctx -> Arith_solver.answer) ->
  Type_ctx.loc_ctx ->
  Heap.heap ->
  Syntax.term ->
  run_result list
