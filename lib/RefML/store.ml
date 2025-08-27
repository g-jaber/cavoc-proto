type store = Syntax.val_env * Heap.heap * Type_ctx.cons_ctx
type location = Loc of Syntax.loc | Cons of Syntax.constructor

(*TODO: We should also print the other components *)
let pp_store fmt (_, heap, _) = Heap.pp_heap fmt heap
let string_of_store = Format.asprintf "%a" pp_store

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

module Storectx = struct
  type t = Type_ctx.loc_ctx * Type_ctx.cons_ctx
  type name = location
  type typ = Types.typ

  let pp fmt (loc_ctx, cons_ctx) =
    if Util.Pmap.is_empty cons_ctx then
      Format.fprintf fmt "%a" Type_ctx.pp_loc_ctx loc_ctx
    else
      Format.fprintf fmt "%a ; %a" Type_ctx.pp_loc_ctx loc_ctx
        Type_ctx.pp_cons_ctx cons_ctx

  let to_string = Format.asprintf "%a" pp

  let to_yojson (loc_ctx, cons_ctx) =
    `List
      [
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (loc, ty) ->
                 (Syntax.string_of_loc loc, `String (Types.string_of_typ ty)))
               loc_ctx);
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (cons, ty) ->
                 ( Syntax.string_of_constructor cons,
                   `String (Types.string_of_typ ty) ))
               cons_ctx);
      ]

  let empty = (Type_ctx.empty_loc_ctx, Type_ctx.empty_cons_ctx)

  let concat (loc_ctx1, cons_ctx1) (loc_ctx2, cons_ctx2) =
    let loc_ctx = Util.Pmap.concat loc_ctx1 loc_ctx2 in
    let cons_ctx = Util.Pmap.concat cons_ctx1 cons_ctx2 in
    (loc_ctx, cons_ctx)

  let get_names (loc_ctx, cons_ctx) =
    let loc_l = List.map (fun l -> Loc l) (Util.Pmap.dom loc_ctx) in
    let cons_l = List.map (fun c -> Cons c) (Util.Pmap.dom cons_ctx) in
    loc_l @ cons_l

  let lookup_exn ((loc_ctx, cons_ctx) : t) (loc : location) =
    match loc with
    | Loc l -> Util.Pmap.lookup_exn l loc_ctx
    | Cons c -> Util.Pmap.lookup_exn c cons_ctx

  let is_empty ((loc_ctx, cons_ctx) : t) =
    Util.Pmap.is_empty loc_ctx && Util.Pmap.is_empty cons_ctx

  let is_singleton ((loc_ctx, cons_ctx) : t) (loc : location) (ty : typ) =
    match loc with
    | Loc l -> Util.Pmap.is_singleton loc_ctx (l, ty)
    | Cons c -> Util.Pmap.is_singleton cons_ctx (c, ty)

  let is_last ((_loc_ctx, _cons_ctx) : t) (_loc : location) (_ty : typ) =
    failwith "TODO"

  let add ((loc_ctx, cons_ctx) : t) (loc : location) (ty : typ) =
    match loc with
    | Loc l -> (Util.Pmap.add (l, ty) loc_ctx, cons_ctx)
    | Cons c -> (loc_ctx, Util.Pmap.add (c, ty) cons_ctx)

  let to_pmap ((loc_ctx, cons_ctx) : t) =
    let loc_ctx' = Util.Pmap.map_dom (fun l -> Loc l) loc_ctx in
    let cons_ctx' = Util.Pmap.map_dom (fun c -> Cons c) cons_ctx in
    Util.Pmap.concat loc_ctx' cons_ctx'

  let mem ((loc_ctx, cons_ctx) : t) = function
    | Loc l -> Util.Pmap.mem l loc_ctx
    | Cons c -> Util.Pmap.mem c cons_ctx

  let singleton _ =
    failwith "Singleton not relevant for store typing context. Please report."

  let add_fresh _ =
    failwith "add_fresh not relevant for store typing context. Please report."

  let map f (loc_ctx, cons_ctx) =
    (Util.Pmap.map_im f loc_ctx, Util.Pmap.map_im f cons_ctx)
end

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
