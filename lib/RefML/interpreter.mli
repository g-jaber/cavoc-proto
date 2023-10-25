type opconf = Syntax.exprML * Heap.heap
type mem_state = opconf list
type 'a m = mem_state -> 'a list * mem_state

val lookup : opconf -> bool m
val add : opconf -> unit m
val interpreter :
  (Syntax.exprML * Heap.heap -> (Syntax.exprML * Heap.heap) m) ->
  Syntax.exprML * Heap.heap -> (Syntax.exprML * Heap.heap) m
val compute_nf_monad : opconf -> (Syntax.exprML * Heap.heap) m
val compute_nf : opconf -> (Syntax.exprML * Heap.heap) option
