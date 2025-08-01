open Util.Pmap

(* id are used for both variables and names*)
type id = string
type constructor = string

let pp_id = Format.pp_print_string
let pp_constructor = Format.pp_print_string
let string_of_id x = x
let string_of_constructor cons = cons

(* loc is used for locations *)
type loc = int

let pp_loc fmt = Format.fprintf fmt "ℓ%d"
let string_of_loc l = "l" ^ string_of_int l

(* we provide a way to generate fresh locations *)
let count_loc = ref 0

let fresh_loc () =
  let l = !count_loc in
  count_loc := !count_loc + 1;
  l

type label = LocL of loc | ConsL of constructor

(* we also provide fresh generation of variable identifiers,
   that is used in the parser to replace some anonymous construction like () or _ *)
let count_evar = ref 0

let fresh_evar () =
  let x = !count_evar in
  count_evar := !count_evar + 1;
  "_y" ^ string_of_int x

(* Syntax of Expressions *)

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
    (* We should generalize constructors so that it takes a list of arguments *)
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

let pp_pattern fmt = function
  | PatCons (c, id) -> Format.fprintf fmt "%s %s" c id
  | PatVar id -> Format.pp_print_string fmt id

let pp_typed_var fmt = function
  | (x, Types.TUndef) -> Format.pp_print_string fmt x
  | (x, ty) -> Format.fprintf fmt "(%s:%a)" x Types.pp_typ ty

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

let string_of_unary_op = function Not -> "not"
let pp_binary_op fmt op = Format.pp_print_string fmt (string_of_binary_op op)
let pp_unary_op fmt op = Format.pp_print_string fmt (string_of_unary_op op)

let rec pp_par_term fmt = function
  | Var x -> pp_id fmt x
  | Loc l -> pp_loc fmt l
  | Unit -> Format.pp_print_string fmt "()"
  | Int n -> Format.pp_print_int fmt n
  | e -> Format.fprintf fmt "(%a)" pp_term e

and pp_term fmt = function
  | Var x -> pp_id fmt x
  | Constructor (c, e) -> Format.fprintf fmt "%a %a" pp_constructor c pp_term e
  | Name n -> Names.pp_name fmt n
  | Loc l -> pp_loc fmt l
  | Unit -> Format.pp_print_string fmt "()"
  | Int n -> Format.pp_print_int fmt n
  | Bool b -> Format.pp_print_bool fmt b
  | BinaryOp (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" pp_term e1 pp_binary_op op pp_term e2
  | UnaryOp (op, e) -> Format.fprintf fmt "%a%a" pp_unary_op op pp_term e
  | If (e1, e2, e3) ->
      Format.fprintf fmt "if %a then %a else %a" pp_term e1 pp_term e2 pp_term
        e3
  | Fun (typedvar, e) ->
      Format.fprintf fmt "fun %a -> %a" pp_typed_var typedvar pp_term e
  | Fix (typedvar1, typedvar2, e) ->
      Format.fprintf fmt "fix %a %a -> %a" pp_typed_var typedvar1 pp_typed_var
        typedvar2 pp_term e
  | Let (var, e1, e2) ->
      Format.fprintf fmt "let %a = %a in %a" pp_id var pp_term e1 pp_term e2
  | LetPair (var1, var2, e1, e2) ->
      Format.fprintf fmt "let (%a,%a) = %a in %a" pp_id var1 pp_id var2 pp_term
        e1 pp_term e2
  | Seq (e1, e2) -> Format.fprintf fmt "%a; %a" pp_term e1 pp_term e2
  | While (e1, e2) ->
      Format.fprintf fmt "while %a do %a done" pp_term e1 pp_term e2
  | App (e1, e2) -> Format.fprintf fmt "%a %a" pp_par_term e1 pp_par_term e2
  | Pair (e1, e2) -> Format.fprintf fmt "(%a,%a)" pp_term e1 pp_term e2
  | Newref (_, e) -> Format.fprintf fmt "ref %a" pp_term e
  | Deref e -> Format.fprintf fmt "!%a" pp_term e
  | Assign (e1, e2) -> Format.fprintf fmt "%a := %a" pp_term e1 pp_term e2
  | Assert e -> Format.fprintf fmt "assert %a" pp_term e
  | Raise e -> Format.fprintf fmt "raise %a" pp_term e
  | TryWith (e, handler_l) ->
      let pp_sep fmt () = Format.pp_print_string fmt "|" in
      let pp_handler_l = Format.pp_print_list ~pp_sep pp_handler in
      Format.fprintf fmt "try %a with %a" pp_term e pp_handler_l handler_l
  | Hole -> Format.pp_print_string fmt "∙"
  | Error -> Format.pp_print_string fmt "error"

and pp_handler fmt (Handler (pat, expr)) =
  Format.fprintf fmt "%a -> %a" pp_pattern pat pp_term expr

let string_of_term = Format.asprintf "%a" pp_term
let pp_value = pp_term
let string_of_value = string_of_term

(* TODO: We should rather use a Set rather than a list to represent set of names*)

type name_set = Names.name list

let empty_name_set = []

let rec get_new_names lnames = function
  | Name n -> if List.mem n lnames then lnames else n :: lnames
  | Var _ | Loc _ | Unit | Int _ | Bool _ | Hole | Error -> lnames
  | Constructor (_, e)
  | UnaryOp (_, e)
  | Fun (_, e)
  | Fix (_, _, e)
  | Newref (_, e)
  | Deref e
  | Assert e ->
      get_new_names lnames e
  | BinaryOp (_, e1, e2)
  | Let (_, e1, e2)
  | LetPair (_, _, e1, e2)
  | Seq (e1, e2)
  | While (e1, e2)
  | App (e1, e2)
  | Pair (e1, e2)
  | Assign (e1, e2) ->
      let lnames1 = get_new_names lnames e1 in
      get_new_names lnames1 e2
  | If (e1, e2, e3) ->
      let lnames1 = get_new_names lnames e1 in
      let lnames2 = get_new_names lnames1 e2 in
      get_new_names lnames2 e3
  | Raise e1 -> get_new_names lnames e1
  | TryWith (e1, handler_l) ->
      let lnames' = get_new_names lnames e1 in
      List.fold_left
        (fun lnames (Handler (_, expr)) -> get_new_names lnames expr)
        lnames' handler_l

let get_names = get_new_names empty_name_set

type label_set = label list

let empty_label_set = []

let rec get_new_labels label_l = function
  | Loc l -> if List.mem (LocL l) label_l then label_l else LocL l :: label_l
  | Constructor (c, _) ->
      if List.mem (ConsL c) label_l then label_l else ConsL c :: label_l
  | Name _ | Var _ | Unit | Int _ | Bool _ | Hole | Error -> label_l
  | UnaryOp (_, e)
  | Fun (_, e)
  | Fix (_, _, e)
  | Newref (_, e)
  | Deref e
  | Assert e ->
      get_new_labels label_l e
  | BinaryOp (_, e1, e2)
  | Let (_, e1, e2)
  | LetPair (_, _, e1, e2)
  | Seq (e1, e2)
  | While (e1, e2)
  | App (e1, e2)
  | Pair (e1, e2)
  | Assign (e1, e2) ->
      let label_l1 = get_new_labels label_l e1 in
      get_new_labels label_l1 e2
  | If (e1, e2, e3) ->
      let label_l1 = get_new_labels label_l e1 in
      let label_l2 = get_new_labels label_l1 e2 in
      get_new_labels label_l2 e3
  | Raise e1 -> get_new_labels label_l e1
  | TryWith (e1, handler_l) ->
      let label_l' = get_new_labels label_l e1 in
      List.fold_left
        (fun label_l (Handler (_, expr)) -> get_new_labels label_l expr)
        label_l' handler_l

let get_labels = get_new_labels empty_label_set

type value = term

let rec isval = function
  (*| Var _ -> true*)
  | Constructor (_, e) -> isval e
  | Name _ -> true
  | Loc _ -> true
  | Unit -> true
  | Int _ -> true
  | Bool _ -> true
  | Fix _ -> true
  | Fun _ -> true
  | Pair (e1, e2) -> isval e1 && isval e2
  | _ -> false

let get_value expr = if isval expr then Some expr else None

let rec subst expr value value' =
  match expr with
  | Var _ when expr = value -> value'
  | Constructor (_, _) when expr = value -> value'
  | Name _ when expr = value -> value'
  | Loc _ when expr = value -> value'
  | Hole when expr = value -> value'
  | Var _ | Name _ | Loc _ | Hole | Unit | Int _ | Bool _ | Error -> expr
  | Constructor (cons, expr') -> Constructor (cons, subst expr' value value')
  | BinaryOp (op, expr1, expr2) ->
      BinaryOp (op, subst expr1 value value', subst expr2 value value')
  | UnaryOp (op, expr) -> UnaryOp (op, subst expr value value')
  | If (expr1, expr2, expr3) ->
      If
        ( subst expr1 value value',
          subst expr2 value value',
          subst expr3 value value' )
  | Fun ((var', ty), expr') when Var var' <> value ->
      Fun ((var', ty), subst expr' value value')
  | Fun _ -> expr
  | Fix ((idfun, tyf), (var', tyv), expr')
    when Var var' <> value && Var idfun <> value ->
      Fix ((idfun, tyf), (var', tyv), subst expr' value value')
  | Fix _ -> expr
  | Let (var', expr1, expr2) when Var var' <> value ->
      Let (var', subst expr1 value value', subst expr2 value value')
  | Let (var', expr1, expr2) -> Let (var', subst expr1 value value', expr2)
  | LetPair (var1, var2, expr1, expr2)
    when Var var1 <> value && Var var2 <> value ->
      LetPair (var1, var2, subst expr1 value value', subst expr2 value value')
  | LetPair (var1, var2, expr1, expr2) ->
      LetPair (var1, var2, subst expr1 value value', expr2)
  | App (expr1, expr2) ->
      App (subst expr1 value value', subst expr2 value value')
  | Seq (expr1, expr2) ->
      Seq (subst expr1 value value', subst expr2 value value')
  | While (expr1, expr2) ->
      While (subst expr1 value value', subst expr2 value value')
  | Pair (expr1, expr2) ->
      Pair (subst expr1 value value', subst expr2 value value')
  | Newref (ty, expr') -> Newref (ty, subst expr' value value')
  | Deref expr' -> Deref (subst expr' value value')
  | Assign (expr1, expr2) ->
      Assign (subst expr1 value value', subst expr2 value value')
  | Assert expr -> Assert (subst expr value value')
  | Raise expr -> Raise (subst expr value value')
  | TryWith (expr, handler_l) ->
      let expr' = subst expr value value' in
      let aux (Handler (pat, expr_pat)) =
        match pat with
        | PatCons _ -> Handler (pat, subst expr_pat value value')
        | PatVar id when Var id <> value ->
            Handler (pat, subst expr_pat value value')
        | PatVar _ -> Handler (pat, expr_pat) in
      TryWith (expr', List.map aux handler_l)

let subst_var expr id = subst expr (Var id)

let subst_list expr lsubst =
  List.fold_left
    (fun expr (var, value) -> subst expr (Var var) value)
    expr lsubst

(* Auxiliary functions *)

let implement_arith_op = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )
  | Div -> ( / )
  | op ->
      failwith
        ("The binary operator " ^ string_of_binary_op op
       ^ " is not an arithmetic operator.")

let implement_bin_bool_op = function
  | And -> ( && ) (* We probably loose lazy semantics *)
  | Or -> ( || )
  | op ->
      failwith
        ("The binary operator " ^ string_of_binary_op op
       ^ " is not a boolean operator.")

let implement_compar_op = function
  | Equal -> ( = )
  | NEqual -> ( <> )
  | Less -> ( < )
  | LessEq -> ( <= )
  | Great -> ( > )
  | GreatEq -> ( >= )
  | op ->
      failwith
        ("The binary operator " ^ string_of_binary_op op
       ^ " is not a comparison operator.")

let get_consfun_from_bin_cons = function
  | BinaryOp (op, _, _) -> fun (x, y) -> BinaryOp (op, x, y)
  | Seq _ -> fun (x, y) -> Seq (x, y)
  | App _ -> fun (x, y) -> App (x, y)
  | Pair _ -> fun (x, y) -> Pair (x, y)
  | Assign _ -> fun (x, y) -> Assign (x, y)
  | expr ->
      failwith
        ("No binary constructor function can be extracted from "
       ^ string_of_term expr)

let get_consfun_from_un_cons = function
  | UnaryOp (op, _) -> fun x -> UnaryOp (op, x)
  | Newref (ty, _) -> fun x -> Newref (ty, x)
  | Deref _ -> fun x -> Deref x
  | Assert _ -> fun x -> Assert x
  | expr ->
      failwith
        ("No unary constructor function can be extracted from "
       ^ string_of_term expr)

(* Full Expressions *)

type val_env = (id, value) pmap

let string_of_val_env = string_of_pmap "ε" "↪" string_of_id string_of_value
let empty_val_env = Util.Pmap.empty

(* Evaluation Contexts *)

type eval_context = term

let pp_eval_context = pp_term
let string_of_eval_context = Format.asprintf "%a" pp_eval_context

(* extract_ctx decomposes an expression into its redex and the surrounding evaluation context*)
let rec extract_ctx expr =
  match expr with
  | Name _ | Loc _ | Unit | Int _ | Bool _ | Fix _ | Fun _ | Error ->
      (expr, Hole)
  | BinaryOp (_, expr1, expr2)
  | App (expr1, expr2)
  | Pair (expr1, expr2)
  | Assign (expr1, expr2) ->
      let consfun = get_consfun_from_bin_cons expr in
      extract_ctx_bin consfun expr1 expr2
  | UnaryOp (_, expr') | Newref (_, expr') | Deref expr' ->
      let consfun = get_consfun_from_un_cons expr in
      extract_ctx_un consfun expr'
  | If (expr1, expr2, expr3) ->
      extract_ctx_un (fun x -> If (x, expr2, expr3)) expr1
  | Let (var, expr1, expr2) ->
      extract_ctx_un (fun x -> Let (var, x, expr2)) expr1
  | LetPair (var1, var2, expr1, expr2) ->
      extract_ctx_un (fun x -> LetPair (var1, var2, x, expr2)) expr1
  | Seq (expr1, expr2) -> extract_ctx_un (fun x -> Seq (x, expr2)) expr1
  | While (expr1, expr2) -> extract_ctx_un (fun x -> While (x, expr2)) expr1
  | Assert expr' -> extract_ctx_un (fun x -> Assert x) expr'
  | Raise expr' -> extract_ctx_un (fun x -> Raise x) expr'
  | Constructor (cons, expr') ->
      extract_ctx_un (fun x -> Constructor (cons, x)) expr'
  | TryWith (expr', handler_l) ->
      extract_ctx_un (fun x -> TryWith (x, handler_l)) expr'
  | Var _ | Hole ->
      failwith
        ("Error: trying to extract an evaluation context from "
       ^ string_of_term expr ^ ". Please report.")

and extract_ctx_bin cons_op expr1 expr2 =
  match (isval expr1, isval expr2) with
  | (false, _) ->
      let (res, ctx) = extract_ctx expr1 in
      (res, cons_op (ctx, expr2))
  | (_, false) ->
      let (res, ctx) = extract_ctx expr2 in
      (res, cons_op (expr1, ctx))
  | (true, true) -> (cons_op (expr1, expr2), Hole)

and extract_ctx_un cons_op expr =
  if isval expr then (cons_op expr, Hole)
  else
    let (result, ctx) = extract_ctx expr in
    (result, cons_op ctx)

let fill_hole ctx expr = subst ctx Hole expr

type negative_val = value

let pp_negative_val = pp_value
let string_of_negative_val = Format.asprintf "%a" pp_negative_val

let filter_negative_val = function
  | (Fix _ | Fun _ | Name _) as value -> Some value
  | _ -> None

let force_negative_val value = value
let embed_negative_val value = value

type interactive_env = (Names.name, negative_val) Util.Pmap.pmap

let interactive_env_to_yojson ienv =
  let to_string (nn, nval) =
    (Names.string_of_name nn, `String (string_of_negative_val nval)) in
  `Assoc (Util.Pmap.to_list @@ Util.Pmap.map to_string ienv)

let empty_ienv = Util.Pmap.empty
let concat_ienv = Util.Pmap.concat

let pp_ienv fmt ienv =
  let pp_sep fmt () = Format.pp_print_char fmt ',' in
  let pp_empty fmt () = Format.pp_print_string fmt "⋅" in
  let pp_pair fmt (n, nval) =
    Format.fprintf fmt "%a ↦ %a" Names.pp_name n pp_negative_val nval in
  Util.Pmap.pp_pmap ~pp_sep ~pp_empty pp_pair fmt ienv

let string_of_ienv = Format.asprintf "%a" pp_ienv

open Nf

let get_nf_term term =
  match get_value term with
  | Some value -> NFValue ((), value)
  | None ->
      let (term', ectx) = extract_ctx term in
      begin
        match term' with
        | Raise v -> begin
            match get_value v with
            | Some value -> NFRaise ((), value)
            | None ->
                failwith @@ "The term " ^ string_of_term term'
                ^ " is not a value. Please report."
          end
        | Error -> NFError ()
        | App (Name fn, v) -> begin
            match get_value v with
            | Some value -> NFCallback (fn, value, ectx)
            | None ->
                failwith @@ "The term " ^ string_of_term term'
                ^ " is not a value. Please report."
          end
        | _ ->
            failwith @@ "The term " ^ string_of_term term
            ^ " is not a valid normal form. Its decomposition is "
            ^ string_of_term term' ^ " and "
            ^ string_of_eval_context ectx
            ^ ". Please report."
      end

let refold_nf_term = function
  | NFCallback (nval, value, ()) -> App (nval, value)
  | NFValue (ectx, value) -> fill_hole ectx value
  | NFError ectx -> fill_hole ectx Error
  | NFRaise (ectx, value) -> fill_hole ectx (Raise value)

let max_int = 1

let generate_ground_value : Types.typ -> term list = function
  | TUnit -> [ Unit ]
  | TBool -> [ Bool true; Bool false ]
  | TInt ->
      let rec aux i = if i < 0 then [] else Int i :: aux (i - 1) in
      aux max_int
  | ty ->
      failwith
        ("Error: the type" ^ Types.string_of_typ ty
       ^ " is not of ground type. It should not appear inside heaps.")
