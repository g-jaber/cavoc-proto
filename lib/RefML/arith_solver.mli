type answer = Sat | Unsat | Unknown

module type ARITH_SOLVER = sig
  val check_sat : Symbolic.symbolic_ctx -> Logic.arith_ctx -> answer end

module Syntactic : ARITH_SOLVER
