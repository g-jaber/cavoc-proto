type memory = Syntax.val_env * Heap.heap

let string_of_memory (valenv, heap) =
  let heap_string = Heap.string_of_heap heap in
  if valenv = Util.Pmap.empty then heap_string
  else
    let valenv_string = Syntax.string_of_val_env valenv in
    heap_string ^ "| " ^ valenv_string

let empty_memory = (Syntax.empty_val_env, Heap.emptyheap)
let loc_lookup (_, heap) loc = Heap.lookup heap loc
let var_lookup (varenv, _) var = Util.Pmap.lookup var varenv

let loc_allocate (valenv, heap) value =
  let (loc, heap') = Heap.allocate heap value in
  (loc, (valenv, heap'))

let loc_modify (valenv, heap) loc value =
  let heap' = Heap.modify heap loc value in
  (valenv, heap')

let var_add (valenv,heap) varval = 
  let valenv' = Util.Pmap.add varval valenv in
  (valenv',heap)

type memory_type_ctx = Type_ctx.loc_ctx

let empty_memory_type_ctx = Type_ctx.empty_loc_ctx

let string_of_memory_type_ctx loc_ctx =
  Type_ctx.string_of_loc_ctx loc_ctx

let concat_memory_ctx loc_ctx1 loc_ctx2 =
  Util.Pmap.concat loc_ctx1 loc_ctx2

let infer_type_memory (_, heap) = Heap.loc_ctx_of_heap heap

let update_memory (valenv, heap1) (_, heap2) = (valenv, Heap.update heap1 heap2)
(*We suppose that valenv is immutable.*)

let restrict loc_ctx (_,heap) = 
  let heap' = Heap.restrict loc_ctx heap in
  (Util.Pmap.empty,heap')