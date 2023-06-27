val iter : int -> ('a -> 'a) -> 'a -> 'a

val fresh_locvar : unit -> string

type symbheap = (Syntax.id, Syntax.exprML) Pmap.pmap

val string_of_symb_heap : symbheap -> string

val fresh_lvar : unit -> string

val fresh_bvar : unit -> string

type arith_pred =
    ATrue
  | AFalse
  | AAnd of arith_pred list
  | AOr of arith_pred list
  | AEqual of Syntax.exprML * Syntax.exprML
  | ANEqual of Syntax.exprML * Syntax.exprML
  | ALess of Syntax.exprML * Syntax.exprML
  | ALessEq of Syntax.exprML * Syntax.exprML
  | AGreat of Syntax.exprML * Syntax.exprML
  | AGreatEq of Syntax.exprML * Syntax.exprML
  | ARel of string * Syntax.exprML list

val get_consfun_from_binpred :
  arith_pred -> Syntax.exprML * Syntax.exprML -> arith_pred
val get_consfun_from_polyadpred : arith_pred -> arith_pred list -> arith_pred
val negate_arith_pred : arith_pred -> arith_pred
val simplify_arith_pred : arith_pred -> arith_pred
val trivially_false : arith_pred list -> bool
val expr_to_arith_pred : Syntax.exprML -> arith_pred
val string_of_conj : string -> ('a -> string) -> 'a list -> string

val string_of_arith_pred : arith_pred -> string

val full_arith_simplification_aux : arith_pred list -> arith_pred list
val full_arith_simplification : arith_pred -> arith_pred


