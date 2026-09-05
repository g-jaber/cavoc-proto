type label = Syntax.label

type store =
  { valenv : Syntax.val_env
  ; heap : Heap.heap
  ; symbolic_ctx : Symbolic.branch
  ; cons_ctx : Type_ctx.cons_ctx
  } [@@deriving to_yojson]

(*TODO: We should also print the other components *)
let pp_store fmt { heap ; symbolic_ctx ; _ } =
  Format.fprintf fmt
    "<@[<v>heap: %a@ pathdecl: [@[<v>%a@]]@ pathcond: [@[<v>%a@]]@]>"
    Heap.pp_heap heap
    Symbolic.pp_pathdecl symbolic_ctx.pathdecl
    Symbolic.pp_pathcond symbolic_ctx.pathcond
let string_of_store = Format.asprintf "%a" pp_store

let empty_store =
  { valenv = Syntax.empty_val_env
  ; heap = Heap.emptyheap
  ; symbolic_ctx = Symbolic.empty
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

module Storectx = struct
  (* TODO: This should really be a record *)
  type t = Type_ctx.loc_ctx * Symbolic.symbolic_ctx * Type_ctx.cons_ctx

  open Syntax

  module Names = struct
    type name = Syntax.label [@@deriving to_yojson]

    let pp_name fmt = function
      | LocL l -> Syntax.pp_loc fmt l
      | SymL id -> Symbolic.pp_id fmt id
      | ConsL c -> Syntax.pp_constructor fmt c

    let string_of_name = function
      | LocL l -> Syntax.string_of_loc l
      | SymL id -> Symbolic.string_of_id id
      | ConsL c -> Syntax.string_of_constructor c

    let is_callable _ = false
    let is_cname _ = false
  end

  type typ = Types.typ

  let pp fmt (loc_ctx, symbolic_ctx, cons_ctx) =
    if Util.Pmap.is_empty cons_ctx then
      Format.fprintf fmt "%a ; %a" Type_ctx.pp_loc_ctx loc_ctx
        Symbolic.pp_pathdecl symbolic_ctx
    else
      Format.fprintf fmt "%a ; %a ; %a" Type_ctx.pp_loc_ctx loc_ctx
        Type_ctx.pp_cons_ctx cons_ctx
        Symbolic.pp_pathdecl symbolic_ctx

  let to_string = Format.asprintf "%a" pp

  let to_yojson (loc_ctx, symbolic_ctx, cons_ctx) =
    `List
      [
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (loc, ty) ->
                 (Syntax.string_of_loc loc, Types.typ_to_yojson ty))
               loc_ctx);
        Symbolic.symbolic_ctx_to_yojson symbolic_ctx ;
        `Assoc
          (Util.Pmap.to_list
          @@ Util.Pmap.map
               (fun (cons, ty) ->
                 ( Syntax.string_of_constructor cons,
                   Types.typ_to_yojson ty ))
               cons_ctx);
      ]

  let empty = (Type_ctx.empty_loc_ctx, Symbolic.empty_symbolic_ctx, Type_ctx.empty_cons_ctx)

  let concat (loc_ctx1, symbolic_ctx1, cons_ctx1) (loc_ctx2, symbolic_ctx2, cons_ctx2) =
    let loc_ctx = Util.Pmap.concat loc_ctx1 loc_ctx2 in
    let symbolic_ctx = Symbolic.union_ctx symbolic_ctx1 symbolic_ctx2 in
    let cons_ctx = Util.Pmap.concat cons_ctx1 cons_ctx2 in
    (loc_ctx, symbolic_ctx, cons_ctx)

  let get_names (loc_ctx, symbolic_ctx, cons_ctx) =
    let loc_l = List.map (fun l -> LocL l) (Util.Pmap.dom loc_ctx) in
    let sym_l = List.map (fun (id, _) -> SymL id) symbolic_ctx in
    let cons_l = List.map (fun c -> ConsL c) (Util.Pmap.dom cons_ctx) in
    loc_l @ sym_l @ cons_l

  let lookup_exn ((loc_ctx, symbolic_ctx, cons_ctx) : t) (loc : label) =
    match loc with
    | LocL l -> Util.Pmap.lookup_exn l loc_ctx
    | SymL id -> List.assoc id symbolic_ctx
    | ConsL c -> Util.Pmap.lookup_exn c cons_ctx

  let is_empty ((loc_ctx, symbolic_ctx, cons_ctx) : t) =
    Util.Pmap.is_empty loc_ctx
    && List.is_empty symbolic_ctx
    && Util.Pmap.is_empty cons_ctx

  let is_singleton ((loc_ctx, symbolic_ctx, cons_ctx) : t) (loc : label) (ty : typ) =
    match loc with
    | LocL l -> Util.Pmap.is_singleton loc_ctx (l, ty)
    | SymL id -> symbolic_ctx = [ id, ty ]
    | ConsL c -> Util.Pmap.is_singleton cons_ctx (c, ty)

  let is_last ((_loc_ctx, _symbolic_ctx, _cons_ctx) : t) (_loc : label) (_ty : typ) =
    failwith "TODO"

  let to_pmap ((loc_ctx, symbolic_ctx, cons_ctx) : t) =
    let loc_ctx' = Util.Pmap.map_dom (fun l -> LocL l) loc_ctx in
    let symbolic_ctx' = Util.Pmap.list_to_pmap (List.map (fun (id, ty) -> (SymL id, ty)) symbolic_ctx) in
    let cons_ctx' = Util.Pmap.map_dom (fun c -> ConsL c) cons_ctx in
    Util.Pmap.concat (Util.Pmap.concat loc_ctx'  symbolic_ctx') cons_ctx'

  let singleton _ =
    failwith "Singleton not relevant for store typing context. Please report."

  let add_fresh _ =
    failwith "add_fresh not relevant for store typing context. Please report."

  let show_name_in _ = Names.string_of_name
  let erase_display_hints = Fun.id

  let map f (loc_ctx, symbolic_ctx, cons_ctx) =
    let loc_ctx' = Util.Pmap.map_im f loc_ctx in
    let symbolic_ctx' = List.map (fun (id, ty) -> (id, f ty)) symbolic_ctx in
    let cons_ctx' = Util.Pmap.map_im f cons_ctx in
    (loc_ctx', symbolic_ctx', cons_ctx')
end

let infer_type_store { heap ; symbolic_ctx = { pathdecl ; _ } ; cons_ctx ; _ } =
  (Heap.loc_ctx_of_heap heap, pathdecl, cons_ctx)

(* We assume that store2 does not contain any constraints. *)
let update_store store1 store2 =
  let heap = Heap.update store1.heap store2.heap in
  let symbolic_ctx = Symbolic.extend_symbolic_ctx store1.symbolic_ctx store2.symbolic_ctx.pathdecl in
  let cons_ctx = Util.Pmap.concat store1.cons_ctx store2.cons_ctx in
  { store1 with heap ; symbolic_ctx ; cons_ctx }
  (* We suppose that valenv is immutable. *)

(* TODO: not sure when restrict and restrict_ctx are called
         and whether the variables declared in the storectx
         should be exported to the returned store *)
(* The constraints of the branch are kept *)
let restrict (loc_ctx, symbolic_ctx, cons_ctx) store =
  let heap = Heap.restrict loc_ctx store.heap in
  let symbolic_ctx = { store.symbolic_ctx with pathdecl = symbolic_ctx } in
  { empty_store with heap ; symbolic_ctx ; cons_ctx }

let is_equiv_store ~compare_heaps store1 store2 =
  ((not compare_heaps) || Util.Pmap.equal store1.heap store2.heap)
  && Util.Pmap.equal store1.cons_ctx store2.cons_ctx

let restrict_ctx (loc_ctx, symbolic_ctx, cons_ctx) label_l =
  let check_typed = function
    | Syntax.LocL l when not (Util.Pmap.mem l loc_ctx) ->
        failwith
          ("Error: the disclosed location " ^ Syntax.string_of_loc l
         ^ "is not of ground type.")
    | _ -> () in
  List.iter check_typed label_l;
  let loc_ctx' =
    Util.Pmap.filter_dom (fun l -> List.mem (Syntax.LocL l) label_l) loc_ctx
  in
  let cons_ctx' =
    Util.Pmap.filter_dom (fun c -> List.mem (Syntax.ConsL c) label_l) cons_ctx
  in
  (loc_ctx', symbolic_ctx, cons_ctx')
