type typevar = string
type id = string

type typ =
  | TUnit
  | TInt
  | TBool
  | TArrow of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ
  | TRef of typ
  | TExn
  | TVar of typevar
  | TForall of typevar list * typ
  | TId of id (* Implementation is only known by Proponent.  *)
  | TName of
      id (* Generated dynamically while instantiating Forall quantifiers. *)
  | TUndef
(* Used to represent the absence of type annotation in fun and fix terms *)

val string_of_typ : typ -> string
val pp_typ : Format.formatter -> typ -> unit
val fresh_typevar : unit -> typ
val fresh_typename : unit -> id
val get_free_tvars : typ -> typevar list
val pp_tvar_l : Format.formatter -> typevar list -> unit

val subst_type : typevar -> typ -> typ -> typ
val generalize_type : typ -> typ

type type_subst = (typevar, typ) Util.Pmap.pmap
val empty_type_subst : type_subst
val apply_type_subst : typ -> type_subst -> typ
val compose_type_subst : type_subst -> type_subst -> type_subst

type type_env = (id, typ) Util.Pmap.pmap
val empty_type_env : type_env
val apply_type_env : typ -> type_env -> typ

val mgu_type : type_env -> typ * typ -> type_subst option

val refresh_forall : typ -> typ


type negative_type = typ

val pp_negative_type : Format.formatter -> negative_type -> unit
val get_negative_type : typ -> negative_type option
val force_negative_type : typ -> negative_type
val string_of_negative_type : negative_type -> string
val get_input_type : negative_type -> typevar list * typ
val get_output_type : negative_type -> typ
