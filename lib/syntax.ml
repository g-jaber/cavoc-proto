open Pmap

type id = string
type loc = int

let string_of_id x = x

let string_of_loc l = "ℓ" ^ (string_of_int l)

let count_loc = ref 0
let fresh_loc () =
  let l = !count_loc in
  count_loc := !count_loc + 1; 
  l

(* Expressions *)

let count_evar = ref 0
let fresh_evar () =
  let x = !count_evar in
  count_evar := !count_evar + 1;("_y" ^ (string_of_int x))

  
type name = 
  | FName of id
  | CName of id

let name_of_id id = FName ("_"^id) 
let count_fname = ref 0
let fresh_fname () =
  let fn = !count_fname in
  count_fname := !count_fname + 1; FName ("f" ^ (string_of_int fn))

let count_cname = ref 0
let fresh_cname () =
  let cn = !count_cname in
  count_cname := !count_cname + 1; CName ("c" ^ (string_of_int cn))

let string_of_name = function
  | FName f -> f
  | CName c -> c 

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
  | Name of name
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
  | Newref of Types.typeML * exprML
  | Deref of exprML
  | Assign of exprML * exprML
  | Assert of exprML
  | Hole
  | ECtx of exprML
  | Named of name * exprML

let get_names expr =
  let rec aux lnames = function
    | Name n -> if List.mem n lnames then lnames else n::lnames
    | Var _ | Loc _ | Unit | Int _ | Bool _ | Hole -> lnames
    | UnaryOp (_,e) | Fun (_,e) | Fix (_,_,e) | Newref (_,e) 
    | Deref e | Assert e | ECtx (e)| Named (_,e) -> aux lnames e
    | BinaryOp (_,e1,e2) | Let (_,e1,e2) | LetPair (_,_,e1,e2) | Seq (e1,e2) 
    | While (e1,e2) | App (e1,e2) | Pair (e1,e2) | Assign (e1,e2) -> 
      let lnames1 = aux lnames e1 in 
      aux lnames1 e2
    | If (e1,e2,e3) -> 
      let lnames1 = aux lnames e1 in
      let lnames2 = aux lnames1 e2 in
      aux lnames2 e3
  in aux [] expr 
      

type valML = exprML

let rec isval = function
  | Var _ -> true
  | Name _ -> true
  | Loc _ -> true
  | Unit -> true
  | Int _ -> true
  | Bool _ -> true
  | Fix _ -> true
  | Fun _ -> true
  | Pair (e1,e2) -> (isval e1) && (isval e2)
  | ECtx _ -> true
  | _ -> false

let rec subst expr value value' = match expr with
  | Var _ when (expr = value) -> value'
  | Name _ when (expr = value) -> value'
  | Loc _ when (expr = value) -> value'
  | Hole when (expr = value) -> value'
  | Var _ | Name _ | Loc _ | Hole | Unit | Int _ | Bool _ -> expr
  | BinaryOp (op,expr1,expr2) ->
    BinaryOp (op,subst expr1 value value', subst expr2 value value')
  | UnaryOp (op,expr) ->
    UnaryOp (op,subst expr value value')
  | If (expr1,expr2,expr3) ->
    If (subst expr1 value value', subst expr2 value value',
        subst expr3 value value')
  | Fun ((var',ty),expr') when (Var var' <> value) ->
    Fun ((var', ty), subst expr' value value')
  | Fun _ -> expr
  | Fix ((idfun,tyf),(var',tyv),expr')
    when ((Var var' <> value) && (Var idfun <> value)) ->
    Fix ((idfun, tyf), (var', tyv), subst expr' value value')
  | Fix _ -> expr
  | Let (var',expr1,expr2) when (Var var' <> value) ->
    Let (var', subst expr1 value value', subst expr2 value value')
  | Let (var',expr1,expr2) ->
    Let (var', subst expr1 value value', expr2)
  | LetPair (var1,var2,expr1,expr2)
    when ((Var var1 <> value) && (Var var2 <> value)) ->
    LetPair (var1,var2, subst expr1 value value', subst expr2 value value')
  | LetPair (var1,var2,expr1,expr2) ->
    LetPair (var1,var2, subst expr1 value value', expr2)
  | App (expr1,expr2) ->
    App (subst expr1 value value', subst expr2 value value')
  | Seq (expr1,expr2) ->
    Seq (subst expr1 value value', subst expr2 value value')
  | While (expr1,expr2) ->
    While (subst expr1 value value', subst expr2 value value')
  | Pair (expr1,expr2) ->
    Pair (subst expr1 value value', subst expr2 value value')
  | Newref (ty,expr') ->
    Newref (ty,subst expr' value value')
  | Deref expr' ->
    Deref (subst expr' value value')
  | Assign (expr1,expr2) ->
    Assign (subst expr1 value value', subst expr2 value value')
  | Assert expr ->
    Assert (subst expr value value')
  | ECtx ectx -> ECtx (subst ectx value value') 
  | Named (cn,expr) -> Named (cn,subst expr value value')

let subst_var expr id = subst expr (Var id)

let subst_list expr lsubst =
  List.fold_left (fun expr (var,value) -> subst expr (Var var) value)
    expr lsubst

let string_of_typed_var = function
  | (x,Types.TUndef) -> x
  | (x,ty) -> "(" ^ x ^ ":" ^ (Types.string_of_typeML ty) ^ ")"

let string_of_binary_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | NEqual -> "<>"
  | Less -> "<"
  | LessEq -> "<="
  | Great -> ">"
  | GreatEq -> ">="

let string_of_unary_op = function
  | Not -> "not"

let rec string_par_of_exprML = function
  | Var x -> x
  | Loc l -> "l" ^ (string_of_int l)
  | Unit -> "()"
  | Int n -> string_of_int n
  | e -> "(" ^ (string_of_exprML e) ^ ")" 

and string_of_exprML = function
  | Var x -> x
  | Name n -> string_of_name n
  | Loc l -> "l" ^ (string_of_int l)
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | BinaryOp (op,e1,e2) -> 
    "(" ^ (string_of_exprML e1) ^ (string_of_binary_op op) ^ (string_of_exprML e2) ^ ")"
  | UnaryOp (op,e) -> (string_of_unary_op op) ^ (string_of_exprML e)
  | If (e1,e2,e3) ->
    "if " ^ (string_of_exprML e1) ^ " then " ^ (string_of_exprML e2)
    ^ " else " ^ (string_of_exprML e3)
  | Fun (typedvar,e) ->
    "fun " ^ (string_of_typed_var typedvar) ^ " -> " ^ (string_of_exprML e)
  | Fix (typedvar1,typedvar2,e) ->
    "fix " ^ (string_of_typed_var typedvar1) ^ " "
    ^ (string_of_typed_var typedvar2)
    ^  " -> " ^ (string_of_exprML e)
  | Let (var,e1,e2) ->
    "let " ^ var ^ " = " ^ (string_of_exprML e1) ^ " in "
    ^ (string_of_exprML e2)
  | LetPair (var1,var2,e1,e2) ->
    "let (" ^ var1 ^ "," ^ var2 ^ ") = " ^ (string_of_exprML e1) ^ " in "
    ^ (string_of_exprML e2)
  | Seq (e1,e2) -> (string_of_exprML e1) ^ " ; " ^ (string_of_exprML e2)
  | While (e1,e2) -> "while " ^ (string_of_exprML e1) ^ " do " ^ (string_of_exprML e2) ^ " done "
  | App (e1,e2) -> (string_par_of_exprML e1) ^ " " ^ (string_par_of_exprML e2)
  | Pair (e1,e2) ->
    "(" ^ (string_of_exprML e1) ^ "," ^ (string_of_exprML e2) ^ ")"
  | Newref (ty,e) -> "ref_" ^ (Types.string_of_typeML ty) ^ " " ^ (string_of_exprML e)
  | Deref e -> "!" ^ (string_of_exprML e)
  | Assign (e1,e2) -> (string_of_exprML e1) ^ " := " ^ (string_of_exprML e2)
  | Assert e -> "assert " ^ (string_of_exprML e)
  | Hole -> "•"
  | ECtx (e) -> "cont(" ^ (string_of_exprML e) ^ ")"
  | Named (cn,e) -> "[" ^ (string_of_name cn) ^ "]" ^ (string_of_exprML e)

(* Auxiliary functions *)

let implement_arith_op = function
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
  | op -> failwith ("The binary operator " ^ (string_of_binary_op op) ^ " is not an arithmetic operator.")

let implement_bin_bool_op = function
  | And -> (&&) (* We probably loose lazy semantics *)
  | Or -> (||)
  | op -> failwith ("The binary operator " ^ (string_of_binary_op op) ^ " is not a boolean operator.")

let implement_compar_op = function
  | Equal -> (=)
  | NEqual -> (<>)
  | Less -> (<)
  | LessEq -> (<=)
  | Great -> (>)
  | GreatEq -> (>=)
  | op -> failwith ("The binary operator " ^ (string_of_binary_op op) ^ " is not a comparison operator.")

let get_consfun_from_bin_cons = function
  | BinaryOp (op,_,_) -> fun (x,y) -> BinaryOp (op,x,y)
  | Let (var,_,_) ->  fun (x,y) -> Let (var,x,y)
  | Seq _ ->  fun (x,y) -> Seq (x,y)
  | App _ ->  fun (x,y) -> App (x,y)
  | Pair _ -> fun (x,y) -> Pair (x,y)
  | Assign _ ->  fun (x,y) -> Assign (x,y)
  | expr -> failwith ("No binary constructor function can be extracted from "
                      ^ (string_of_exprML expr))

let get_consfun_from_un_cons = function
  | UnaryOp (op,_) -> fun x -> UnaryOp (op,x) 
  | Newref (ty,_) -> fun x -> Newref (ty,x) 
  | Deref _ -> fun x -> Deref x 
  | expr -> failwith ("No unary constructor function can be extracted from "
                      ^ (string_of_exprML expr))

(* Full Expressions *)

type functional_env = (id,exprML) pmap

let string_of_functional_env = string_of_pmap "ε" "->" string_of_id string_of_exprML

type full_expr = exprML*functional_env

let string_of_full_expr (expr,gamma) =
  "(" ^ (string_of_exprML expr) ^ ",[" ^
  (string_of_pmap "ε" "->" string_of_id string_of_exprML gamma) ^ "])"

(* Evaluation Contexts *)

type eval_context = exprML

let rec extract_ctx expr = match expr with
  | _ when (isval expr) -> (expr,Hole)
  | BinaryOp (_,expr1,expr2) | App (expr1,expr2) | Pair (expr1,expr2) 
  | Assign (expr1,expr2) ->
    let consfun = get_consfun_from_bin_cons expr in
    extract_ctx_bin consfun expr1 expr2
  | UnaryOp (_,expr') | Newref (_,expr') | Deref expr' -> 
    let consfun = get_consfun_from_un_cons expr in
    extract_ctx_un consfun expr'
  | If (expr1,expr2,expr3) ->
    extract_ctx_un (fun x -> If (x,expr2,expr3)) expr1
  | Let (var,expr1,expr2) ->
    extract_ctx_un (fun x -> Let (var,x,expr2)) expr1
  | Seq (expr1,expr2) ->
    extract_ctx_un (fun x -> Seq (x,expr2)) expr1
  | While (expr1,expr2) ->
    extract_ctx_un (fun x -> While (x,expr2)) expr1
  | Named (cn,expr) -> extract_ctx_un (fun x -> Named (cn,x)) expr
  | expr ->
    failwith ("Error: trying to extract an evaluation context from "
              ^ (string_of_exprML expr) ^ ". Please report.")

and extract_ctx_bin cons_op expr1 expr2 =
  match (isval expr1, isval expr2) with
  | (false,_) -> let (res,ctx) = extract_ctx expr1 in
    (res,cons_op (ctx,expr2))
  | (_,false) -> let (res,ctx) = extract_ctx expr2 in
    (res,cons_op (expr1,ctx))
  | (true,true) -> (cons_op (expr1,expr2),Hole)

and extract_ctx_un cons_op expr =
  if (isval expr) then (expr,cons_op Hole)
  else let (result,ctx) = extract_ctx expr in (result, cons_op ctx)

let extract_call expr =
  let (expr',ctx) = extract_ctx expr in
  match expr' with
  | App (Var f,expr'') -> (f,expr'',ctx)
  | _ -> failwith ("Error : " ^ (string_of_exprML expr')
                   ^ " is not a call to a function. Please report.")

let extract_body = function
  | Fun ((var,_),expr) -> (var,(expr,Pmap.empty))
  | Fix ((idfun,_),(var,_),expr) as fullexpr -> (var,(expr,Pmap.singleton (idfun,fullexpr)))
  | expr -> failwith ("Error: " ^ (string_of_exprML expr)
                      ^ " is not a function. Please report.")

let fill_hole ctx expr = subst ctx Hole expr

let string_of_eval_context ctx = string_of_exprML ctx

(* Typing contexts for variables and locations *)

type var_ctx = (id,Types.typeML) Pmap.pmap
type loc_ctx = (loc,Types.typeML) Pmap.pmap

let subst_vctx tvar sty =
  Pmap.map_im (fun ty -> Types.subst_type tvar sty ty)

let lsubst_type lsubst ty =
  Pmap.fold (fun ty (tvar,sty) -> Types.subst_type tvar sty ty) ty lsubst

let lsubst_vctx lsubst =
  Pmap.map_im (fun ty -> lsubst_type lsubst ty)

let string_of_var_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ (Types.string_of_typeML ty)
  in Pmap.string_of_pmap "ε" "" string_of_id aux

let string_of_loc_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ (Types.string_of_typeML ty)
  in Pmap.string_of_pmap "ε" "" string_of_loc aux

type name_ctx = (name,Types.typeML) Pmap.pmap

let string_of_name_ctx =
  let aux = function
    | Types.TUndef -> "undef"
    | ty -> "::" ^ (Types.string_of_typeML ty)
  in Pmap.string_of_pmap "ε" "" string_of_name aux

let empty_name_ctx = Pmap.empty
let empty_loc_ctx = Pmap.empty

let init_term term ty =
  let cn = fresh_cname () in
  (Named (cn,term),Pmap.singleton (cn,Types.TNeg ty))

let max_int = 1

let generate_ground_value : Types.typeML -> exprML list = function
  | TUnit -> [Unit]
  | TBool -> [Bool true; (Bool false)]
  | TInt ->
    let rec aux i =
      if i < 0 then []
      else (Int i)::(aux (i-1))
    in aux max_int
  | ty -> failwith ("Error: the type"
            ^ (Types.string_of_typeML ty)
            ^ " is not of ground type. It should not appear inside heaps.")