type op_conf = Syntax.exprML * Syntax.functional_env * Heap.heap
val red : op_conf -> (op_conf*bool)

val compute_nf : op_conf -> op_conf
