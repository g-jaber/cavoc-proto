type typevar = string
type id = string

type typeML =
  | TUnit
  | TInt
  | TBool
  | TArrow of typeML * typeML
  | TProd of typeML * typeML
  | TSum of typeML * typeML
  | TRef of typeML
  | TVar of typevar
  | TForall of typevar list * typeML
  | TId of id (* Implementation is only known by Proponent.  *)
  | TName of id (* Generated dynamically while instantiating Forall quantifiers. *)
  | TUndef (* Used to represent the absence of type annotation in fun and fix terms *)

val string_of_typeML : typeML -> string
val fresh_typevar : unit -> typeML
val fresh_typename : unit -> id
val get_tvars : typeML -> typevar list

type type_subst = (typevar, typeML) Util.Pmap.pmap
type type_env = (id, typeML) Util.Pmap.pmap

val apply_type_subst : typeML -> type_subst -> typeML
val apply_type_env : typeML -> type_env -> typeML
val subst_type : typevar -> typeML -> typeML -> typeML
val unify_type : type_subst -> typeML * typeML -> (typeML * type_subst) option
val generalize_type : typeML -> typeML