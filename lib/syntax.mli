type id = string
type loc = int

val string_of_loc : loc -> string

val fresh_evar : unit -> id

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

type unary_op =
  | Not

type exprML =
    Var of id
  | Loc of loc
  | Unit
  | Int of int
  | Bool of bool
  | BinaryOp of binary_op*exprML * exprML
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
  | Newref of exprML
  | Deref of exprML
  | Assign of exprML * exprML
  | Assert of exprML
  | Hole
  | Named of id * exprML

val isval : exprML -> bool
val subst : exprML -> exprML -> exprML -> exprML
val subst_list : exprML -> (id * exprML) list -> exprML
val string_of_typed_var : Types.typevar * Types.typeML -> string
val string_par_of_exprML : exprML -> string
val string_of_exprML : exprML -> string

val implement_arith_op : binary_op -> (int -> int -> int)

val implement_bin_bool_op : binary_op -> (bool -> bool -> bool)

val implement_compar_op : binary_op -> (int -> int -> bool)


val get_consfun_from_bin_cons : exprML -> exprML * exprML -> exprML
val get_consfun_from_un_cons : exprML -> exprML -> exprML

type functional_env = (id, exprML) Pmap.pmap

val string_of_functional_env : string -> functional_env -> string

type full_expr = exprML * functional_env

val string_of_full_expr : full_expr -> string

type eval_context = exprML

val extract_ctx : exprML -> exprML * eval_context

val extract_call : exprML -> id * eval_context * exprML

val extract_body : exprML -> (id*full_expr)

val fill_hole : eval_context -> exprML -> exprML

val string_of_eval_context : eval_context -> string

type var_ctx = (id,Types.typeML) Pmap.pmap
type loc_ctx = (loc,Types.typeML) Pmap.pmap


val subst_vctx :
  id ->
  Types.typeML ->
    var_ctx -> var_ctx
val lsubst_type :
  (id, Types.typeML) Pmap.pmap -> Types.typeML -> Types.typeML
val lsubst_vctx :
  (id, Types.typeML) Pmap.pmap ->
  ('a, Types.typeML) Pmap.pmap -> ('a, Types.typeML) Pmap.pmap
val string_of_var_ctx : var_ctx -> string