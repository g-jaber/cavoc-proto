module type TEMPFORM = functor (Store:Lang.Language.STORE) -> sig
type id = string
val get_var : int -> id
  type modality = AG | EX

type formula =
  | True
  | False
  | PredStore of Store.store
  | Mod of modality * formula
  | TAnd of formula list
  | TImpl of formula * formula
  | Var of id

val string_of_formula : formula -> string

val simplify_formula : formula -> formula
end


module Make : TEMPFORM = functor (Store:Lang.Language.STORE) -> struct

(* Temporal Logic *)

type id = string

let get_var n = "X" ^ string_of_int n

type modality = AG | EX

type formula =
  | True
  | False
  | PredStore of Store.store
  | Mod of modality * formula
  | TAnd of formula list
  | TImpl of formula * formula
  | Var of id

let string_of_modality = function AG -> "AG" | EX -> "EX"

let rec string_of_conj sep g = function
  | [] -> ""
  | [ p ] -> g p
  | p :: preds' ->
      let str1 = g p in
      let str2 = string_of_conj sep g preds' in
      str1 ^ sep ^ str2

let rec string_of_formula = function
  | True -> "True"
  | False -> "False"
  | PredStore (store) ->
      "P" ^ Store.string_of_store store
  | Mod (m, formula) ->
      string_of_modality m ^ "(" ^ string_of_formula formula ^ ")"
  | TAnd preds -> string_of_conj " /\\ " string_of_formula preds
  | TImpl (formula, formula') ->
      string_of_formula formula ^ " => " ^ string_of_formula formula'
  | Var x -> x

let rec simplify_formula formula =
  match formula with
  | True | False | PredStore _ | Var _ -> formula
  | Mod (m, formula) -> Mod (m, simplify_formula formula)
  | TAnd [] -> True
  | TAnd [ formula ] -> simplify_formula formula
  | TAnd formulas ->
      let formulas' = List.filter (fun x -> x <> True) formulas in
      let formulas'' = List.map simplify_formula formulas' in
      TAnd formulas''
  | TImpl (True, formula) -> simplify_formula formula
  | TImpl (False, _) -> True
  | TImpl (formula, formula') ->
      TImpl (simplify_formula formula, simplify_formula formula')
  end