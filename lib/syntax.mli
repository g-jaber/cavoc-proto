type id = string
type loc = int


val fresh_evar : unit -> id

type exprML =
    Var of id
  | Loc of loc
  | Unit
  | Int of loc
  | Bool of bool
  | Plus of exprML * exprML
  | Minus of exprML * exprML
  | Mult of exprML * exprML
  | Div of exprML * exprML
  | Not of exprML
  | And of exprML * exprML
  | Or of exprML * exprML
  | Equal of exprML * exprML
  | NEqual of exprML * exprML
  | Less of exprML * exprML
  | LessEq of exprML * exprML
  | Great of exprML * exprML
  | GreatEq of exprML * exprML
  | If of exprML * exprML * exprML
  | Fun of (id * Types.typeML) * exprML
  | Fix of (id * Types.typeML) * (id * Types.typeML) * exprML
  | Let of id * exprML * exprML
  | LetPair of id * id * exprML * exprML
  | App of exprML * exprML
  | Seq of exprML * exprML
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
val string_of_typed_var : id * Types.typeML -> string
val string_par_of_exprML : exprML -> string
val string_of_exprML : exprML -> string

val get_consfun_from_binexpr : exprML -> exprML * exprML -> exprML
val get_consfun_from_unexpr : exprML -> exprML -> exprML

val get_aop_from_expr :
  exprML ->
  exprML * exprML * (loc -> loc -> loc) * (exprML * exprML -> exprML)

val get_abop_from_expr :
  exprML ->
  exprML * exprML * ('a -> 'a -> bool) * (exprML * exprML -> exprML)

val get_bop_from_expr :
  exprML ->
  exprML * exprML * (bool -> bool -> bool) * (exprML * exprML -> exprML)

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
val string_of_var_ctx : var_ctx -> id