(* This file provides an evaluator defined as a small-step reduction relation for RefML *)

type op_conf = Syntax.exprML * Syntax.val_env * Heap.heap

val red : op_conf -> op_conf * bool
val compute_nf : op_conf -> op_conf
