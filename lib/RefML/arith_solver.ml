type answer = Sat | Unsat | Unknown

module type ARITH_SOLVER = sig
  val check_sat : Symbolic.symbolic_ctx -> Logic.arith_ctx -> answer end

module Syntactic = struct
  let check_sat _ constraints =
    let constraints = List.map Logic.simplify_arith_pred constraints in
    match Logic.full_arith_simplification (Logic.AAnd constraints) with
    | Logic.ATrue -> Sat
    | Logic.AFalse -> Unsat
    | Logic.AAnd preds -> if Logic.trivially_false preds then Unsat else Unknown
    | pred -> if Logic.trivially_false [ pred ] then Unsat else Unknown
end
