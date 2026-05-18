type location = Loc of Syntax.loc | Cons of Syntax.constructor [@@deriving to_yojson]

type store =
  { valenv : Syntax.val_env
  ; heap : Heap.heap
  ; branch : Symbolic.branch
  ; cons_ctx : Type_ctx.cons_ctx
  } [@@deriving to_yojson]

(*TODO: We should also print the other components *)
let pp_store fmt { heap ; branch ; _ } =
  Format.fprintf fmt
    "<@[<v>heap: %a@ pathdecl: [@[<v>%a@]]@ pathcond: [@[<v>%a@]]@]>"
    Heap.pp_heap heap
    Symbolic.pp_pathdecl branch.pathdecl
    Symbolic.pp_pathcond branch.pathcond
let string_of_store = Format.asprintf "%a" pp_store

let empty_store =
  { valenv = Syntax.empty_val_env
  ; heap = Heap.emptyheap
  ; branch = Symbolic.empty
  ; cons_ctx = Type_ctx.empty_cons_ctx
  }

let loc_lookup store loc = Heap.lookup store.heap loc
let var_lookup store var = Util.Pmap.lookup var store.valenv
let cons_lookup store cons = Util.Pmap.lookup cons store.cons_ctx

let loc_allocate store value =
  let (loc, heap) = Heap.allocate store.heap value in
  (loc, { store with heap })

let loc_modify store loc value =
  let heap = Heap.modify store.heap loc value in
  { store with heap }

let var_add store varval =
  let valenv = Util.Pmap.add varval store.valenv in
  { store with valenv }

let cons_add store (cons, ty) =
  let cons_ctx = Util.Pmap.add (cons, ty) store.cons_ctx in
  { store with cons_ctx }

let symbolic_add store =
  let sym, branch = Symbolic.unconstrained store.branch in
  sym, { store with branch }

let symbolic_add_named store name ty =
  let sym, store = symbolic_add store in
  var_add store (name, Symbolic (Kvar sym, ty))

let symbolic_add_constraint store konstraint =
  { store with branch = Symbolic.add_constraint store.branch konstraint }

let embed_cons_ctx cons_ctx =
  { empty_store with cons_ctx }

module Storectx = struct
  type t = Type_ctx.loc_ctx * Type_ctx.cons_ctx

  module Names = struct
    type name = location [@@deriving to_yojson]

    let pp_name fmt = function
      | Loc l -> Syntax.pp_loc fmt l
      | Cons c -> Syntax.pp_constructor fmt c

    let string_of_name = function
      | Loc l -> Syntax.string_of_loc l
      | Cons c -> Syntax.string_of_constructor c

    let is_callable _ = false
    let is_cname _ = false
  end

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
                 (Syntax.string_of_loc loc, Types.typ_to_yojson ty))
               loc_ctx);
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (cons, ty) ->
                 ( Syntax.string_of_constructor cons,
                   Types.typ_to_yojson ty ))
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

  let to_pmap ((loc_ctx, cons_ctx) : t) =
    let loc_ctx' = Util.Pmap.map_dom (fun l -> Loc l) loc_ctx in
    let cons_ctx' = Util.Pmap.map_dom (fun c -> Cons c) cons_ctx in
    Util.Pmap.concat loc_ctx' cons_ctx'

  let singleton _ =
    failwith "Singleton not relevant for store typing context. Please report."

  let add_fresh _ =
    failwith "add_fresh not relevant for store typing context. Please report."

  let map f (loc_ctx, cons_ctx) =
    (Util.Pmap.map_im f loc_ctx, Util.Pmap.map_im f cons_ctx)
end

let infer_type_store { heap ; cons_ctx ; _ } =
  (Heap.loc_ctx_of_heap heap, cons_ctx)

let update_store store1 store2 =
  let heap = Heap.update store1.heap store2.heap in
  let cons_ctx = Util.Pmap.concat store1.cons_ctx store2.cons_ctx in
  (* TODO: merge constraints ? *)
  { store1 with heap ; cons_ctx }
  (*We suppose that valenv is immutable.*)

let restrict (loc_ctx, cons_ctx) store =
  let heap = Heap.restrict loc_ctx store.heap in
  { empty_store with heap ; cons_ctx }

type label = Syntax.label

let restrict_ctx (loc_ctx, cons_ctx) label_l =
  let loc_ctx' =
    Util.Pmap.filter_dom (fun l -> List.mem (Syntax.LocL l) label_l) loc_ctx
  in
  let cons_ctx' =
    Util.Pmap.filter_dom (fun c -> List.mem (Syntax.ConsL c) label_l) cons_ctx
  in
  (loc_ctx', cons_ctx')
