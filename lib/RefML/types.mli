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
  | TId of id
  | TNeg of typeML (* type of evaluation contexts *)
  | TUndef

val string_of_typeML : typeML -> string
val fresh_typevar : unit -> typeML
val get_tvars : typeML -> typevar list

type type_subst = (typevar, typeML) Util.Pmap.pmap

val apply_type_subst : typeML -> type_subst -> typeML
val subst_type : typevar -> typeML -> typeML -> typeML
val unify_type : type_subst -> typeML * typeML -> (typeML * type_subst) option
val neg_type : typeML -> typeML
