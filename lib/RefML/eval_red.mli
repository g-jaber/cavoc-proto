(* This file provides an evaluator defined as a small-step reduction relation for RefML *)

type op_conf = Syntax.term * Syntax.val_env * Heap.heap

val red : op_conf -> op_conf * bool
val normalize_opconf : op_conf -> op_conf
