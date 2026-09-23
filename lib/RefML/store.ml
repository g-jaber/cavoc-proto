type label =
  | LocL of Names.LocNames.name
  | ConsL of Syntax.constructor
  | SymL of Symbolic.id
[@@deriving to_yojson]

module LocCtx =
  Lang.Typectx.Make_List
    (Names.LocNames)
    (struct
      type t = Types.typ [@@deriving to_yojson]

      let pp = Types.pp_typ
    end)

type disclosed_locs = (Names.LocNames.name, Syntax.loc) Util.Pmap.pmap

let pp_disclosed_locs fmt disclosed_locs =
  let pp_pair fmt (loc_name, loc) =
    Format.fprintf fmt "%a ↦ %a" Names.LocNames.pp_name loc_name Syntax.pp_loc loc in
  let pp_sep fmt () = Format.fprintf fmt ";" in
  Format.fprintf fmt "[%a]" (Util.Pmap.pp_pmap ~pp_sep pp_pair) disclosed_locs

let disclosed_locs_to_yojson disclosed_locs =
  `Assoc
    (List.map
       (fun (loc_name, loc) ->
         (Names.LocNames.string_of_name loc_name, `String (Syntax.string_of_loc loc)))
       (Util.Pmap.to_list disclosed_locs))

type store =
  { valenv : Syntax.val_env
  ; heap : Heap.heap
  ; symbolic_ctx : Symbolic.branch
  ; cons_ctx : Type_ctx.cons_ctx
  ; loc_ctx : LocCtx.t
  ; disclosed_locs : disclosed_locs
  } [@@deriving to_yojson]

(*TODO: We should also print the other components *)
let pp_store fmt { heap ; symbolic_ctx ; disclosed_locs ; _ } =
  Format.fprintf fmt
    "<@[<v>heap: %a@ public: %a@ pathdecl: [@[<v>%a@]]@ pathcond: [@[<v>%a@]]@]>"
    Heap.pp_heap heap
    pp_disclosed_locs disclosed_locs
    Symbolic.pp_pathdecl symbolic_ctx.pathdecl
    Symbolic.pp_pathcond symbolic_ctx.pathcond
let string_of_store = Format.asprintf "%a" pp_store

let empty_store =
  { valenv = Syntax.empty_val_env
  ; heap = Heap.emptyheap
  ; symbolic_ctx = Symbolic.empty
  ; cons_ctx = Type_ctx.empty_cons_ctx
  ; loc_ctx = LocCtx.empty
  ; disclosed_locs = Util.Pmap.empty
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
  let sym, pathdecl = Symbolic.unconstrained store.symbolic_ctx.pathdecl in
  let symbolic_ctx = { store.symbolic_ctx with pathdecl } in
  sym, { store with symbolic_ctx }

let symbolic_add_named store name _ty =
  let sym, store = symbolic_add store in
  var_add store (name, Symbolic (Kvar sym))

let symbolic_add_constraint store konstraint =
  { store with symbolic_ctx = Symbolic.add_constraint store.symbolic_ctx konstraint }

let embed_cons_ctx cons_ctx =
  { empty_store with cons_ctx }

let loc_name_of_loc store loc =
  match Util.Pmap.select_im loc store.disclosed_locs with
  | loc_name :: _ -> Some loc_name
  | [] -> None

let loc_of_loc_name store loc_name = Util.Pmap.lookup loc_name store.disclosed_locs

let disclose_loc store loc ty =
  let (loc_name, loc_ctx) = LocCtx.add_fresh store.loc_ctx "" ty in
  let disclosed_locs = Util.Pmap.add (loc_name, loc) store.disclosed_locs in
  (loc_name, { store with loc_ctx ; disclosed_locs })

module Storectx = struct
  (* TODO: This should really be a record *)
  type t = LocCtx.t * Symbolic.symbolic_ctx * Type_ctx.cons_ctx

  module Names = struct
    type name = label [@@deriving to_yojson]

    let pp_name fmt = function
      | LocL loc_name -> Names.LocNames.pp_name fmt loc_name
      | SymL id -> Symbolic.pp_id fmt id
      | ConsL c -> Syntax.pp_constructor fmt c

    let string_of_name = Format.asprintf "%a" pp_name
    let is_callable _ = false
    let is_cname _ = false
  end

  type typ = Types.typ

  let pp fmt (loc_ctx, symbolic_ctx, cons_ctx) =
    if Util.Pmap.is_empty cons_ctx then
      Format.fprintf fmt "%a ; %a" LocCtx.pp loc_ctx
        Symbolic.pp_pathdecl symbolic_ctx
    else
      Format.fprintf fmt "%a ; %a ; %a" LocCtx.pp loc_ctx
        Type_ctx.pp_cons_ctx cons_ctx
        Symbolic.pp_pathdecl symbolic_ctx

  let to_string = Format.asprintf "%a" pp

  let to_yojson (loc_ctx, symbolic_ctx, cons_ctx) =
    `List
      [
        LocCtx.to_yojson loc_ctx;
        Symbolic.symbolic_ctx_to_yojson symbolic_ctx ;
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (cons, ty) ->
                 ( Syntax.string_of_constructor cons,
                   Types.typ_to_yojson ty ))
               cons_ctx);
      ]

  let empty = (LocCtx.empty, Symbolic.empty_symbolic_ctx, Type_ctx.empty_cons_ctx)

  let concat (loc_ctx1, symbolic_ctx1, cons_ctx1) (loc_ctx2, symbolic_ctx2, cons_ctx2) =
    let loc_ctx = LocCtx.concat loc_ctx1 loc_ctx2 in
    let symbolic_ctx = Symbolic.union_ctx symbolic_ctx1 symbolic_ctx2 in
    let cons_ctx = Util.Pmap.concat cons_ctx1 cons_ctx2 in
    (loc_ctx, symbolic_ctx, cons_ctx)

  let get_names (loc_ctx, symbolic_ctx, cons_ctx) =
    let loc_l = List.map (fun l -> LocL l) (LocCtx.get_names loc_ctx) in
    let sym_l = List.map (fun (id, _) -> SymL id) symbolic_ctx in
    let cons_l = List.map (fun c -> ConsL c) (Util.Pmap.dom cons_ctx) in
    loc_l @ sym_l @ cons_l

  let lookup_exn ((loc_ctx, symbolic_ctx, cons_ctx) : t) (label : label) =
    match label with
    | LocL loc_name -> LocCtx.lookup_exn loc_ctx loc_name
    | SymL id -> List.assoc id symbolic_ctx
    | ConsL c -> Util.Pmap.lookup_exn c cons_ctx

  let is_empty ((loc_ctx, symbolic_ctx, cons_ctx) : t) =
    LocCtx.is_empty loc_ctx
    && List.is_empty symbolic_ctx
    && Util.Pmap.is_empty cons_ctx

  let is_singleton ((loc_ctx, symbolic_ctx, cons_ctx) : t) (label : label) (ty : typ) =
    match label with
    | LocL loc_name -> LocCtx.is_singleton loc_ctx loc_name ty
    | SymL id -> symbolic_ctx = [ id, ty ]
    | ConsL c -> Util.Pmap.is_singleton cons_ctx (c, ty)

  let is_last ((_loc_ctx, _symbolic_ctx, _cons_ctx) : t) (_label : label) (_ty : typ) =
    failwith "TODO"

  let to_pmap ((loc_ctx, symbolic_ctx, cons_ctx) : t) =
    let loc_ctx' = Util.Pmap.map_dom (fun l -> LocL l) (LocCtx.to_pmap loc_ctx) in
    let symbolic_ctx' = Util.Pmap.list_to_pmap (List.map (fun (id, ty) -> (SymL id, ty)) symbolic_ctx) in
    let cons_ctx' = Util.Pmap.map_dom (fun c -> ConsL c) cons_ctx in
    Util.Pmap.concat (Util.Pmap.concat loc_ctx'  symbolic_ctx') cons_ctx'

  let singleton _ =
    failwith "Singleton not relevant for store typing context. Please report."

  (* Only locations get fresh entries. *)
  let add_fresh (loc_ctx, symbolic_ctx, cons_ctx) str ty =
    let (loc_name, loc_ctx') = LocCtx.add_fresh loc_ctx str ty in
    (LocL loc_name, (loc_ctx', symbolic_ctx, cons_ctx))

  let show_name_in _ = Names.string_of_name
  let erase_display_hints = Fun.id

  let map f (loc_ctx, symbolic_ctx, cons_ctx) =
    let loc_ctx' = LocCtx.map f loc_ctx in
    let symbolic_ctx' = List.map (fun (id, ty) -> (id, f ty)) symbolic_ctx in
    let cons_ctx' = Util.Pmap.map_im f cons_ctx in
    (loc_ctx', symbolic_ctx', cons_ctx')
end

let infer_type_store { loc_ctx ; symbolic_ctx = { pathdecl ; _ } ; cons_ctx ; _ } =
  (loc_ctx, pathdecl, cons_ctx)

let loc_ctx (loc_ctx, _, _) = loc_ctx
let embed_loc_typ ty = ty

let disclose_heap store =
  let disclose store (loc, ty) =
    match loc_name_of_loc store loc with
    | Some _ -> store
    | None -> snd (disclose_loc store loc ty) in
  Util.Pmap.fold disclose store (Heap.loc_ctx_of_heap store.heap)

(* A declaration already present is not repeated. *)
let update_store store1 store2 =
  let heap = Heap.update store1.heap store2.heap in
  let pathdecl =
    List.filter
      (fun (id, _) -> not (List.mem_assoc id store1.symbolic_ctx.pathdecl))
      store2.symbolic_ctx.pathdecl in
  let symbolic_ctx = Symbolic.extend_symbolic_ctx store1.symbolic_ctx pathdecl in
  let cons_ctx =
    Util.Pmap.fold
      (fun cons_ctx (c, ty) ->
        if Util.Pmap.mem c cons_ctx then cons_ctx else Util.Pmap.add (c, ty) cons_ctx)
      store1.cons_ctx store2.cons_ctx in
  { store1 with heap ; symbolic_ctx ; cons_ctx }

let without_heap (_, symbolic_ctx, cons_ctx) store =
  let symbolic_ctx = { store.symbolic_ctx with pathdecl = symbolic_ctx } in
  { empty_store with symbolic_ctx ; cons_ctx }

let is_equiv_store store1 store2 =
  Util.Pmap.equal store1.cons_ctx store2.cons_ctx
