(* This constructor has to be made public, otherwise
   the location returned by Storectx.add_fresh in
   generate_abstract_val is not usable. *)
type location =
  | Loc of Syntax.loc
  | Sym of Symbolic.id
  | Cons of Syntax.constructor [@@deriving to_yojson]

type store =
  { valenv : Syntax.val_env
  ; heap : Heap.heap
  ; branch : Symbolic.branch
  ; cons_ctx : Type_ctx.cons_ctx
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
val symbolic_add_constraint : store -> Symbolic.konstraint -> store

val embed_cons_ctx : Type_ctx.cons_ctx -> store

module Storectx : Lang.Typectx.TYPECTX
  with type t = Type_ctx.loc_ctx * Symbolic.branch_ctx * Type_ctx.cons_ctx
  and type typ = Types.typ
  and  type Names.name = location

val infer_type_store : store -> Storectx.t

(* update_store (env,h) (env',h') is equal to (env,h[h']) *)
val update_store : store -> store -> store
val restrict : Storectx.t -> store -> store

type label = Syntax.label

val restrict_ctx : Storectx.t -> label list -> Storectx.t
