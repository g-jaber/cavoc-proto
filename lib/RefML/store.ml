type store = Syntax.val_env * Heap.heap * Type_ctx.cons_ctx

let string_of_store (_, heap, _) = Heap.string_of_heap heap
  (*let heap_string = Heap.string_of_heap heap in
  if valenv = Util.Pmap.empty then heap_string
  else
    let valenv_string = Syntax.string_of_val_env valenv in
    heap_string ^ "| " ^ valenv_string*)

let empty_store = (Syntax.empty_val_env, Heap.emptyheap, Type_ctx.empty_cons_ctx)
let loc_lookup (_, heap, _) loc = Heap.lookup heap loc
let var_lookup (varenv, _, _) var = Util.Pmap.lookup var varenv
let cons_lookup (_, _, cons_ctx) cons = Util.Pmap.lookup cons cons_ctx

let loc_allocate (valenv, heap, cons_ctx) value =
  let (loc, heap') = Heap.allocate heap value in
  (loc, (valenv, heap', cons_ctx))

let loc_modify (valenv, heap, cons_ctx) loc value =
  let heap' = Heap.modify heap loc value in
  (valenv, heap', cons_ctx)

let var_add (valenv, heap, cons_ctx) varval =
  let valenv' = Util.Pmap.add varval valenv in
  (valenv', heap, cons_ctx)

let cons_add (valenv, heap, cons_ctx) (cons, ty) =
  let cons_ctx' = Util.Pmap.add (cons, ty) cons_ctx in
  (valenv, heap, cons_ctx')

let embed_cons_ctx cons_ctx = (Util.Pmap.empty, Util.Pmap.empty, cons_ctx)

type store_ctx = Type_ctx.loc_ctx * Type_ctx.cons_ctx

let empty_store_ctx = (Type_ctx.empty_loc_ctx, Type_ctx.empty_cons_ctx)

let string_of_store_ctx (loc_ctx, cons_ctx) =
  Type_ctx.string_of_loc_ctx loc_ctx
  ^ ","
  ^ Type_ctx.string_of_cons_ctx cons_ctx

let concat_store_ctx (loc_ctx1, cons_ctx1) (loc_ctx2, cons_ctx2) =
  let loc_ctx = Util.Pmap.concat loc_ctx1 loc_ctx2 in
  let cons_ctx = Util.Pmap.concat cons_ctx1 cons_ctx2 in
  (loc_ctx, cons_ctx)

let infer_type_store (_, heap, cons_ctx) = (Heap.loc_ctx_of_heap heap, cons_ctx)

let update_store (valenv, heap1, cons_ctx1) (_, heap2, cons_ctx2) =
  let heap = Heap.update heap1 heap2 in
  let cons_ctx = Util.Pmap.concat cons_ctx1 cons_ctx2 in
  (valenv, heap, cons_ctx)
(*We suppose that valenv is immutable.*)

let restrict (loc_ctx, cons_ctx) (_, heap, _) =
  let heap' = Heap.restrict loc_ctx heap in
  (Util.Pmap.empty, heap', cons_ctx)

type label = Syntax.label

let restrict_ctx (loc_ctx, cons_ctx) label_l =
  let loc_ctx' =
    Util.Pmap.filter_dom (fun l -> List.mem (Syntax.LocL l) label_l) loc_ctx
  in
  let cons_ctx' =
    Util.Pmap.filter_dom (fun c -> List.mem (Syntax.ConsL c) label_l) cons_ctx
  in
  (loc_ctx', cons_ctx')
