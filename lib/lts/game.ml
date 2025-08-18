module type LTS = sig
  module Moves : Moves.POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  type position [@@deriving to_yojson]

  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* generate_move Γₓ return
      all the pairs (m,Γₓ') such that
      there exists a name context Δ for the free names of m such that
      Γₓ ⊢ m ▷ Δ  and Γₓ' = Γₓ + Δ.
     It uses the branching monad from BranchMonad to do so. *)
  val generate_moves :
    position -> (Moves.pol_move * position) BranchMonad.m

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed.*)
  val check_move : position -> Moves.pol_move -> position option

  (*
  val trigger_move :
    position -> Moves.pol_move -> name_ctx -> store_ctx -> position *)
end
