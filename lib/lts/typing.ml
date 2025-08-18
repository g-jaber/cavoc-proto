module type LTS = sig
  module Moves : Moves.POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  type name_ctx
  type store_ctx

  (* *)

  val domain_of_name_ctx : name_ctx -> Moves.Names.name list

  type position [@@deriving to_yojson]

  val get_namectxO : position -> name_ctx
  val get_storectx : position -> store_ctx
  val init_act_pos : store_ctx -> name_ctx -> name_ctx -> position
  val init_pas_pos : store_ctx -> name_ctx -> name_ctx -> position
  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* generate_move Γₓ return
      all the pairs (m,Γₓ') such that
      there exists a name context Δ for the free names of m such that
      Γₓ ⊢ m ▷ Δ  and Γₓ' = Γₓ + Δ.
     It uses the branching monad from BranchMonad to do so. *)
  val generate_moves :
    position -> ((Moves.pol_move * name_ctx) * position) BranchMonad.m

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed.*)
  val check_move : position -> Moves.pol_move * name_ctx -> position option
  val trigger_move : position -> Moves.pol_move * name_ctx -> position
end