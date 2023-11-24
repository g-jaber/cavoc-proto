val iter : int -> ('a -> 'a) -> 'a -> 'a
val fresh_locvar : unit -> string

type symbheap = (Syntax.id, Syntax.term) Util.Pmap.pmap

val string_of_symb_heap : symbheap -> string
val fresh_lvar : unit -> string
val fresh_bvar : unit -> string

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
  | ARel of string * Syntax.term list

val get_consfun_from_binpred :
  arith_pred -> Syntax.term * Syntax.term -> arith_pred

val get_consfun_from_polyadpred : arith_pred -> arith_pred list -> arith_pred
val negate_arith_pred : arith_pred -> arith_pred
val simplify_arith_pred : arith_pred -> arith_pred
val trivially_false : arith_pred list -> bool
val expr_to_arith_pred : Syntax.term -> arith_pred
val string_of_conj : string -> ('a -> string) -> 'a list -> string
val string_of_arith_pred : arith_pred -> string
val full_arith_simplification_aux : arith_pred list -> arith_pred list
val full_arith_simplification : arith_pred -> arith_pred

type arith_ctx = arith_pred list

val string_of_arith_ctx : arith_ctx -> string
