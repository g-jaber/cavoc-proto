(* id are used for both variables and names*)
type id = string 
type constructor = string 
type label = string 
type opsymbol = string 

let string_of_id x = x
let string_of_constructor cons = cons
let string_of_label l = l
let string_of_opsymbol op = op

let count_evar = ref 0

let fresh_evar () =
  let x = !count_evar in
  count_evar := !count_evar + 1;
  "_y" ^ string_of_int x

(* Syntax of terms *)

type pattern = 
  | PatVar of id
  | PatName of Names.name
  | PatUnit
  | PatInt of int
  | PatBool of bool
  | PatCons of constructor * pattern list 
  | PatRecord of (label * pattern) list
  | PatVariant of label * pattern
 

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

type value =
  | Var of id
  | Constructor of constructor * value list
  | Name of Names.name
  | Unit
  | Int of int
  | Bool of bool
  | Binop of binary_op * value * value
  | Record of (label * value) list
  | Variant of (label * value)
  | Lambda of (id * Types.vtyp) * computation
 

and computation =
  | Return of value
  | Let of id * computation * computation
  | Match of value * (pattern * computation) list
  | App of value * value
  | Handle of computation * handler
  | Perform of opsymbol * value
 

and handler = {
  ocs : (opsymbol * (id * id * computation)) list;
  ret : id * computation
} 

let id_handler = let x = fresh_evar() in {ocs=[]; ret=(x, Return (Var x))}




type name_set = Names.name list

let empty_name_set = []

let rec string_of_pattern = function 
  | PatVar id -> id
  | PatName s -> Names.string_of_name s
  | PatUnit -> "()"
  | PatInt n -> string_of_int n
  | PatBool b -> string_of_bool b
  | PatCons (c, pat_l) -> 
     c ^ List.fold_left (fun acc pat -> acc ^ " " ^ string_of_pattern pat) "" pat_l
  | PatRecord lab_pat_l -> "<" ^ 
      (List.fold_left (fun acc (lab, pat) -> acc ^ lab ^ ":" ^ string_of_pattern pat ^ "; ") "" lab_pat_l)
      ^ ">"
  | PatVariant (lab, pat) -> lab ^ " " ^ string_of_pattern pat

let string_of_typed_var = function
  | (x, Types.TUndef) -> x
  | (x, ty) -> "(" ^ x ^ ":" ^ Types.string_of_vtyp ty ^ ")"

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

let rec string_of_value = function
  | Var x -> x
  | Constructor (c, val_l) -> 
    c ^ List.fold_left (fun acc -> fun value -> acc ^ " " ^ string_of_value value ) "" val_l
  | Name n -> Names.string_of_name n
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Lambda (typedvar, comp) ->
      "λ" ^ string_of_typed_var typedvar ^ " -> " ^ string_of_computation comp
  | Binop (binop, val1, val2) -> string_of_value val1 ^ " " ^ string_of_binary_op binop ^ " " ^ string_of_value val2
  | Record lab_val_l -> "{" ^
     (List.fold_left (fun acc (lab, value) -> acc ^ lab ^ ":" ^ string_of_value value ^ "; ") "" lab_val_l)
     ^ "}" 
  | Variant (lab, value) -> lab ^ " " ^ string_of_value value

and string_par_of_value = function
  | Var x -> x
  | Constructor (c, val_l) -> 
    c ^ List.fold_left (fun acc -> fun value -> acc ^ " " ^ string_of_value value ) "" val_l
  | Name n -> Names.string_of_name n
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Record lab_val_l -> "{" ^
     (List.fold_left (fun acc (lab, value) -> acc ^ lab ^ ":" ^ string_of_value value ^ "; ") "" lab_val_l)
     ^ "}" 
  | Variant (lab, value) -> lab ^ " " ^ string_of_value value
  | v -> "("^ string_of_value v ^")"

and string_of_computation = function
  | Return value -> "return " ^ string_of_value value
  | Let (var, e1, e2) -> 
      "let " ^ var ^ " = " ^ string_of_computation e1 ^ " in " ^ string_of_computation e2
  | Match (value, pcs) -> "match " ^ string_of_value value ^ " with " ^ "{" ^string_of_match_cases pcs ^ "}"
  | App (v1, v2) -> string_par_of_value v1 ^ " " ^ string_par_of_value v2
  | Handle (e, h) -> "handle " ^ string_of_computation e ^ " with " ^ "{" ^ string_of_handler h ^ "}"
  | Perform (opsym, value) -> opsym ^ " " ^ string_of_value value

and string_of_match_cases mcs_l = 
  List.fold_left (fun acc (pat, e) -> 
   acc ^ " | " ^ string_of_pattern pat ^ " ⇒ " ^ string_of_computation e) "" mcs_l

and string_of_handler h = 
  string_of_ocs h.ocs ^ " | " ^ string_of_retcs h.ret 

and string_of_ocs ocs_l = 
  List.fold_left (fun acc (op, (var, cont, e)) -> 
  acc ^ op ^ " " ^ var ^ " " ^ cont ^ " ⇒ " ^ string_of_computation e) "" ocs_l
 
and string_of_retcs = function 
  (var, e) -> "return " ^ var ^ " ⇒ " ^ string_of_computation e 
   


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


(* On names *)

module NameSet = Set.Make(
  struct 
    type t = Names.name
    let compare = Names.compare
  end)

let rec get_names = function
  | Name n -> NameSet.singleton n
  | Var _   | Unit | Int _ | Bool _  -> NameSet.empty
  | Constructor (_, val_l) ->
    List.fold_left 
     (fun names value -> NameSet.union (get_names value) names)
      NameSet.empty val_l
  | Binop (_, val1, val2) -> 
    NameSet.union (get_names val1) (get_names val2)
  | Record labelled_vals -> 
    List.fold_left 
     (fun names (_,value) -> NameSet.union (get_names value) names)
      NameSet.empty labelled_vals
  | Variant (_, value) -> get_names value
  | Lambda (_, comp) -> get_names_comp comp

and get_names_comp = function
  | Return v -> get_names v
  | Let (_, comp1, comp2) ->
     NameSet.union (get_names_comp comp1) (get_names_comp comp2)
  | Match (v, match_cases) -> 
    let branches_names = 
     List.fold_left 
      (fun names (_,comp) -> NameSet.union (get_names_comp comp) names)
       NameSet.empty match_cases in 
     NameSet.union (get_names v) branches_names 
  | App (v1, v2) -> NameSet.union (get_names v1) (get_names v2)
  | Handle (comp, h) -> 
     let h_names = 
      List.fold_left 
       (fun names (_,(_, _,comp)) -> NameSet.union (get_names_comp comp) names)
        (get_names_comp (snd h.ret)) h.ocs in 
      NameSet.union (get_names_comp comp) h_names 
  | Perform (_, v) -> get_names v


(* free variables *)

module VarSet = Set.Make(
  struct 
    type t = id 
    let compare = String.compare
end)

let rec get_vars_pat = function
  | PatVar id -> VarSet.singleton id
  | PatName _ | PatUnit | PatInt _ | PatBool _ -> VarSet.empty
  | PatCons (_, pat_l) -> 
     List.fold_left 
      (fun vars pat -> VarSet.union (get_vars_pat pat) vars) 
       VarSet.empty pat_l
  | PatRecord lab_pat_l ->
      List.fold_left 
      (fun vars (_, pat) -> VarSet.union (get_vars_pat pat) vars)
       VarSet.empty lab_pat_l
  | PatVariant (_, pat) -> get_vars_pat pat
