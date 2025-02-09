open Syntax

let rec iter n f x =
  match n with
  | 0 -> x
  | _ ->
      let y = f x in
      iter (n - 1) f y

let count_locvar = ref 0

let fresh_locvar () =
  let l = !count_locvar in
  count_locvar := !count_locvar + 1;
  "ℓ" ^ string_of_int l

(* Symbolic Heaps *)

type symbheap = (id, term) Util.Pmap.pmap

let string_of_symb_heap =
  Util.Pmap.string_of_pmap "ε" "↪" Syntax.string_of_id Syntax.string_of_term

let count_lvar = ref 0

let fresh_lvar () =
  let x = !count_lvar in
  count_lvar := !count_lvar + 1;
  "x" ^ string_of_int x

let count_bvar = ref 0

let fresh_bvar () =
  let x = !count_bvar in
  count_bvar := !count_bvar + 1;
  "b" ^ string_of_int x

type arith_pred =
  | ATrue
  | AFalse
  | AAnd of arith_pred list
  | AOr of arith_pred list
  | AEqual of Syntax.term * Syntax.term
  | ANEqual of Syntax.term * Syntax.term
  | ALess of Syntax.term * Syntax.term
  | ALessEq of Syntax.term * Syntax.term
  | AGreat of Syntax.term * Syntax.term
  | AGreatEq of Syntax.term * Syntax.term
  | ARel of Syntax.id * Syntax.term list

let get_consfun_from_binpred = function
  | AEqual _ -> fun (x, y) -> AEqual (x, y)
  | ANEqual _ -> fun (x, y) -> ANEqual (x, y)
  | ALess _ -> fun (x, y) -> ALess (x, y)
  | ALessEq _ -> fun (x, y) -> ALessEq (x, y)
  | AGreat _ -> fun (x, y) -> AGreat (x, y)
  | AGreatEq _ -> fun (x, y) -> AGreatEq (x, y)
  | _ ->
      failwith "No binary constructor function can be extracted. Please report."

let get_consfun_from_polyadpred = function
  | AAnd _ -> fun preds -> AAnd preds
  | AOr _ -> fun preds -> AOr preds
  | _ ->
      failwith
        "No polyadic constructor function can be extracted. Please report."

let rec negate_arith_pred = function
  | ATrue -> AFalse
  | AFalse -> ATrue
  | AAnd preds -> AOr (List.map negate_arith_pred preds)
  | AOr preds -> AAnd (List.map negate_arith_pred preds)
  | AEqual (e1, e2) -> ANEqual (e1, e2)
  | ANEqual (e1, e2) -> AEqual (e1, e2)
  | ALess (e1, e2) -> AGreatEq (e1, e2)
  | ALessEq (e1, e2) -> AGreat (e1, e2)
  | AGreat (e1, e2) -> ALessEq (e1, e2)
  | AGreatEq (e1, e2) -> ALess (e1, e2)
  | ARel _ -> failwith "Cannot negate a relation. Please report."

let rec simplify_arith_pred = function
  | AAnd [] -> ATrue
  | AAnd [ pred ] -> pred
  | AAnd preds ->
      let preds' = List.filter (fun x -> x <> ATrue) preds in
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem AFalse preds'' then AFalse else AAnd preds''
  | AOr [] -> AFalse
  | AOr [ pred ] -> pred
  | AOr preds ->
      let preds' = List.filter (fun x -> x <> AFalse) preds in
      let preds'' = List.map simplify_arith_pred preds' in
      if List.mem ATrue preds'' then ATrue else AOr preds''
  | AEqual (expr1, expr2) when expr1 = expr2 -> ATrue
  | ANEqual (expr1, expr2) when expr1 = expr2 -> AFalse
  | ALess (expr1, expr2) when expr1 = expr2 -> AFalse
  | ALessEq (expr1, expr2) when expr1 = expr2 -> ATrue
  | AGreat (expr1, expr2) when expr1 = expr2 -> AFalse
  | AGreatEq (expr1, expr2) when expr1 = expr2 -> ATrue
  | pred -> pred

let trivially_false preds =
  (*  let preds = List.map (fun pred -> iter 3  simplify_arith_pred pred) preds in*)
  List.mem AFalse preds
  || List.fold_left
       (fun b pred -> b || List.mem (negate_arith_pred pred) preds)
       false preds

let expr_to_arith_pred = function
  | Bool true -> ATrue
  | Bool false -> AFalse
  | BinaryOp (Equal, expr1, expr2) -> AEqual (expr1, expr2)
  | BinaryOp (NEqual, expr1, expr2) -> ANEqual (expr1, expr2)
  | BinaryOp (Less, expr1, expr2) -> ALess (expr1, expr2)
  | BinaryOp (LessEq, expr1, expr2) -> ALessEq (expr1, expr2)
  | BinaryOp (Great, expr1, expr2) -> AGreat (expr1, expr2)
  | BinaryOp (GreatEq, expr1, expr2) -> AGreatEq (expr1, expr2)
  | expr ->
      failwith
        ("Error: trying to transform the expression " ^ string_of_term expr
       ^ " into a predicate.")

let rec string_of_conj sep g = function
  | [] -> ""
  | [ p ] -> g p
  | p :: preds' ->
      let str1 = g p in
      let str2 = string_of_conj sep g preds' in
      str1 ^ sep ^ str2

let rec string_of_arith_pred = function
  | ATrue -> "True"
  | AFalse -> "False"
  | AAnd preds -> "(" ^ string_of_conj " ∧ " string_of_arith_pred preds ^ ")"
  | AOr preds -> "(" ^ string_of_conj " ∨ " string_of_arith_pred preds ^ ")"
  | AEqual (e1, e2) -> string_of_term e1 ^ " = " ^ string_of_term e2
  | ANEqual (e1, e2) -> string_of_term e1 ^ " ≠ " ^ string_of_term e2
  | ALess (e1, e2) -> string_of_term e1 ^ " < " ^ string_of_term e2
  | ALessEq (e1, e2) -> string_of_term e1 ^ " ≤ " ^ string_of_term e2
  | AGreat (e1, e2) -> string_of_term e1 ^ " > " ^ string_of_term e2
  | AGreatEq (e1, e2) -> string_of_term e1 ^ " ≥ " ^ string_of_term e2
  | ARel (f, lexpr) ->
      f ^ "(" ^ String.concat "," (List.map string_of_term lexpr) ^ ")"

let rec full_arith_simplification_aux = function
  | [] -> []
  | (AEqual (e1, e2) as apred) :: preds ->
      if List.mem (ANEqual (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | (ANEqual (e1, e2) as apred) :: preds ->
      if List.mem (AEqual (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | (ALess (e1, e2) as apred) :: preds ->
      if List.mem (AGreatEq (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | (ALessEq (e1, e2) as apred) :: preds ->
      if List.mem (AGreat (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | (AGreat (e1, e2) as apred) :: preds ->
      if List.mem (ALessEq (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | (AGreatEq (e1, e2) as apred) :: preds ->
      if List.mem (ALess (e1, e2)) preds then [ AFalse ]
      else
        apred
        :: List.filter
             (fun x -> x <> apred)
             (full_arith_simplification_aux preds)
  | apred :: preds ->
      apred
      :: List.filter (fun x -> x <> apred) (full_arith_simplification_aux preds)

let full_arith_simplification apred =
  match apred with
  | AAnd preds ->
      simplify_arith_pred (AAnd (full_arith_simplification_aux preds))
  | _ -> apred

type arith_ctx = arith_pred list

let string_of_arith_ctx actx =
  let strlist = List.map string_of_arith_pred actx in
  let str = String.concat "/\\" strlist in
  "[" ^ str ^ "]"
