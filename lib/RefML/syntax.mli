type id = string
type loc

val string_of_id : id -> string
val string_of_loc : loc -> string
val fresh_loc : unit -> loc
val fresh_evar : unit -> id

type name

val fresh_fname : unit -> name
val fresh_cname : unit -> name
val fresh_pname : unit -> name
val string_of_name : name -> string
val fname_of_id : id -> name
val cname_of_id : id -> name
val cname_to_id : name -> id option
val is_callable : name -> bool

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

type exprML =
  | Var of id
  | Name of name
  | Loc of loc
  | Unit
  | Int of int
  | Bool of bool
  | BinaryOp of binary_op * exprML * exprML
  | UnaryOp of unary_op * exprML
  | If of exprML * exprML * exprML
  | Fun of (id * Types.typeML) * exprML
  | Fix of (id * Types.typeML) * (id * Types.typeML) * exprML
  | Let of id * exprML * exprML
  | LetPair of id * id * exprML * exprML
  | App of exprML * exprML
  | Seq of exprML * exprML
  | While of exprML * exprML
  | Pair of exprML * exprML
  | Newref of Types.typeML * exprML
  | Deref of exprML
  | Assign of exprML * exprML
  | Assert of exprML
  | Hole
  | ECtx of exprML
  | Named of name * exprML

type name_set = name list
val empty_name_set : name_set
val get_new_names : name_set -> exprML -> name_set
val get_names : exprML -> name_set

type valML = exprML

val isval : exprML -> bool
(* The following function subst expr value value 'can be used to substitue any occurence of 
  value by value' in expr.
   It is particulatly useful to substitute names.*)
val subst : exprML -> valML -> valML -> exprML
val subst_var : exprML -> id -> valML -> exprML
val subst_list : exprML -> (id * valML) list -> exprML
val string_of_typed_var : Types.typevar * Types.typeML -> string
val string_par_of_exprML : exprML -> string
val string_of_exprML : exprML -> string
val implement_arith_op : binary_op -> int -> int -> int
val implement_bin_bool_op : binary_op -> bool -> bool -> bool
val implement_compar_op : binary_op -> int -> int -> bool
val get_consfun_from_bin_cons : exprML -> exprML * exprML -> exprML
val get_consfun_from_un_cons : exprML -> exprML -> exprML

type val_env = (id, valML) Util.Pmap.pmap

val string_of_val_env : val_env -> string
val empty_val_env : val_env

type full_expr = exprML * val_env

val string_of_full_expr : full_expr -> string

type eval_context = exprML

val extract_ctx : exprML -> exprML * eval_context
val extract_call : exprML -> id * eval_context * exprML
val extract_body : exprML -> id * full_expr
val fill_hole : eval_context -> exprML -> exprML
val string_of_eval_context : eval_context -> string
val generate_ground_value : Types.typeML -> valML list
