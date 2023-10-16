open Syntax
open Logic

(* Temporal Logic *)

let get_svar n = "X" ^ (string_of_int n)

type modality =
  | Square
  | SquarePub
  | NextWB of symbheap*symbheap*symbheap*symbheap
  | NextE of symbheap*symbheap*symbheap*symbheap
  | NextI of symbheap*symbheap*symbheap*symbheap


type temp_formula =
  | TPred of arith_pred
  | Mod of modality * temp_formula
  | TAnd of temp_formula list
  | TImpl of (arith_pred list)*temp_formula
  | TForAll of (Syntax.var_ctx*temp_formula)
  | GFix of id * temp_formula
  | SVar of id

let string_of_modality = function
  | Square -> "▫V"
  | SquarePub -> "▫K"
  | NextE (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○e_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"
  | NextI (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○I_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"
  | NextWB (heapPre1,heapPre2,heapPost1,heapPost2) ->
      "○wb_(" ^ ( string_of_symb_heap heapPre1) ^ "," ^( string_of_symb_heap heapPre2) ^ "," ^( string_of_symb_heap heapPost1) ^ ","
      ^ ( string_of_symb_heap heapPost2) ^ ")"

let rec string_of_temp_formula = function
  | TPred pred -> string_of_arith_pred pred
  | Mod (m,formula) -> (string_of_modality m) ^ "(" ^ (string_of_temp_formula formula) ^ ")"
  | TAnd  preds-> string_of_conj  " /\\ " string_of_temp_formula preds
  | TImpl (preds,formula) -> (string_of_conj " /\\ " string_of_arith_pred preds) ^ " => " ^ (string_of_temp_formula formula)
  | TForAll (vctx,formula) -> "∀" ^ (string_of_var_ctx vctx) ^ ", " ^ (string_of_temp_formula formula)
  | GFix (x, formula) -> "nu " ^ x ^ "." ^ (string_of_temp_formula formula)
  | SVar (x) -> x


let rec simplify_temp_formula formula = match formula with
  | TPred pred -> TPred (simplify_arith_pred pred)
  | Mod (m,formula) -> Mod (m,simplify_temp_formula formula)
  | TAnd [] -> TPred ATrue
  | TAnd [formula] -> simplify_temp_formula formula
  | TAnd formulas ->
      let formulas' = List.filter (fun x-> x<>TPred ATrue) formulas in
      let formulas'' = List.map simplify_temp_formula formulas' in
      TAnd formulas''
  | TImpl ([],formula) | TImpl ([ATrue],formula) -> simplify_temp_formula formula
  | TImpl ([AFalse],_) -> TPred ATrue
  | TImpl (preds,formula) ->
      let preds' = List.map simplify_arith_pred preds in
      TImpl (preds',simplify_temp_formula formula)
  | TForAll (vctx,formula) -> 
    let formula' = simplify_temp_formula formula in
    if Pmap.is_empty vctx then formula'
    else TForAll (vctx,formula')
  | GFix (x, formula) -> GFix (x, simplify_temp_formula formula)
  | SVar _ -> formula