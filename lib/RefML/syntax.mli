type id = string
type constructor = string
type loc

val pp_id : Format.formatter -> id -> unit
val pp_constructor : Format.formatter -> constructor -> unit
val pp_loc : Format.formatter -> loc -> unit
val string_of_id : id -> string
val string_of_constructor : constructor -> string
val string_of_loc : loc -> string
val fresh_loc : unit -> loc

type label = LocL of loc | ConsL of constructor

val fresh_evar : unit -> id

type pattern = PatCons of constructor * id | PatVar of id

type binary_op =
  | Plus
  | Minus
  | Mult
  | Div
  | And
  | Or
  | Equal
  | NEqual
  | Less
  | LessEq
  | Great
  | GreatEq

type unary_op = Not

type handler = Handler of (pattern * term)

and term =
  | Var of id
  | Constructor of constructor * term
  | Name of Names.name
  | Loc of loc
  | Unit
  | Int of int
  | Bool of bool
  | BinaryOp of binary_op * term * term
  | UnaryOp of unary_op * term
  | If of term * term * term
  | Fun of (id * Types.typ) * term
  | Fix of (id * Types.typ) * (id * Types.typ) * term
  | Let of id * term * term
  | LetPair of id * id * term * term
  | App of term * term
  | Seq of term * term
  | While of term * term
  | Pair of term * term
  | Newref of Types.typ * term
  | Deref of term
  | Assign of term * term
  | Assert of term
  | Raise of term
  | TryWith of (term * handler list)
  | Hole
  | Error

val pp_term : Format.formatter -> term -> unit
val string_of_term : term -> string

type name_set = Names.name list

val empty_name_set : name_set
val get_new_names : name_set -> term -> name_set
val get_names : term -> name_set

type label_set = label list

val empty_label_set : label_set
val get_new_labels : label_set -> term -> label_set
val get_labels : term -> label_set

type value = term

val pp_value : Format.formatter -> value -> unit
val string_of_value : value -> string
val isval : term -> bool

(* The following function subst expr value value 'can be used to substitue any occurence of
   value by value' in expr. The second argument value can either be a variable, a Names.name, a location or the Hole.*)
val subst : term -> value -> value -> term
val subst_var : term -> id -> value -> term
val subst_list : term -> (id * value) list -> term
val implement_arith_op : binary_op -> int -> int -> int
val implement_bin_bool_op : binary_op -> bool -> bool -> bool
val implement_compar_op : binary_op -> int -> int -> bool
val get_consfun_from_bin_cons : term -> term * term -> term
val get_consfun_from_un_cons : term -> term -> term

type val_env = (id, value) Util.Pmap.pmap

val string_of_val_env : val_env -> string
val empty_val_env : val_env

type eval_context

val pp_eval_context : Format.formatter -> eval_context -> unit
val string_of_eval_context : eval_context -> string

type negative_val

val pp_negative_val : Format.formatter -> negative_val -> unit
val string_of_negative_val : negative_val -> string
val filter_negative_val : value -> negative_val option
val force_negative_val : value -> negative_val
val embed_negative_val : negative_val -> value
val get_nf_term : term -> (value, eval_context, Names.name, unit) Nf.nf_term

val refold_nf_term :
  (value, unit, negative_val, eval_context) Nf.nf_term -> term

type interactive_env = (Names.name, negative_val) Util.Pmap.pmap

val empty_ienv : interactive_env
val concat_ienv : interactive_env -> interactive_env -> interactive_env
val string_of_ienv : interactive_env -> string
val pp_ienv : Format.formatter -> interactive_env -> unit

(* The following function should be replaced by generate_nup *)
val generate_ground_value : Types.typ -> value list
