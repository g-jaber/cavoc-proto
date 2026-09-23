type label =
  | LocL of Names.LocNames.name
  | ConsL of Syntax.constructor
  | SymL of Symbolic.id
[@@deriving to_yojson]

(* The store typing of the disclosed locations: Σ(l) = τ for l : ref τ. *)
module LocCtx : Lang.Typectx.TYPECTX
  with type typ = Types.typ
  and type Names.name = Names.LocNames.name

type disclosed_locs = (Names.LocNames.name, Syntax.loc) Util.Pmap.pmap

(* loc_ctx and disclosed_locs give each disclosed location its name and its
   type; evaluation never reads them. *)
type store =
  { valenv : Syntax.val_env
  ; heap : Heap.heap
  ; symbolic_ctx : Symbolic.branch
  ; cons_ctx : Type_ctx.cons_ctx
  ; loc_ctx : LocCtx.t
  ; disclosed_locs : disclosed_locs
  } [@@deriving to_yojson]

val string_of_store : store -> string
val pp_store : Format.formatter -> store -> unit
val empty_store : store
val loc_lookup :  store -> Syntax.loc -> Syntax.value option
val var_lookup :  store -> Syntax.id -> Syntax.value option
val cons_lookup :  store -> Syntax.id -> Types.typ option
val loc_allocate : store -> Syntax.value -> (Syntax.loc*store)
val loc_modify : store ->  Syntax.loc -> Syntax.value -> store
val var_add : store -> (Syntax.id*Syntax.value) -> store
val cons_add : store -> (Syntax.constructor*Types.typ) -> store
(* Add an unconstrained typed symbolic value to the store, returning
   its unique id *)
val symbolic_add : store -> (Symbolic.id * store)
val symbolic_add_named : store -> Syntax.id -> Types.typ -> store
val symbolic_add_constraint : store -> Symbolic.symbolic_expr -> store

val embed_cons_ctx : Type_ctx.cons_ctx -> store

val loc_name_of_loc : store -> Syntax.loc -> Names.LocNames.name option
val loc_of_loc_name : store -> Names.LocNames.name -> Syntax.loc option

(* disclose_loc µ l τ gives l the location name fresh for Σ, typed by τ. *)
val disclose_loc : store -> Syntax.loc -> Types.typ -> Names.LocNames.name * store

module Storectx : Lang.Typectx.TYPECTX
  with type t = LocCtx.t * Symbolic.symbolic_ctx * Type_ctx.cons_ctx
  and type typ = Types.typ
  and type Names.name = label

val infer_type_store : store -> Storectx.t
val loc_ctx : Storectx.t -> LocCtx.t
val embed_loc_typ : Types.typ -> Types.typ
val disclose_heap : store -> store
val update_store : store -> store -> store
val without_heap : Storectx.t -> store -> store
val is_equiv_store : store -> store -> bool
