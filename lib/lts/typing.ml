module type LTS = sig
  module Moves : Moves.POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  type store_ctx
  type position [@@deriving to_yojson]

  val get_namectxO : position -> Moves.Renaming.Namectx.t
  val get_namectxP : position -> Moves.Renaming.Namectx.t
  val get_storectx : position -> store_ctx

  val init_act_pos :
    store_ctx ->
    Moves.Renaming.Namectx.t ->
    Moves.Renaming.Namectx.t ->
    position

  val init_pas_pos :
    store_ctx ->
    Moves.Renaming.Namectx.t ->
    Moves.Renaming.Namectx.t ->
    position

  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* Every transition returns, with the target position, the weakening of
     the local context of the move. *)

  (* generate_moves Γₓ returns all the pairs (m, Γₓ') such that there exists
     a name context Δ for the free names of m with Γₓ ⊢ m ▷ Δ and Γₓ' = Γₓ + Δ,
     using the branching monad. *)
  val generate_moves :
    position -> (Moves.pol_move * Moves.Renaming.t * position) BranchMonad.m

  (* check_move Γₓ m returns None when m is not well-typed at Γₓ. *)
  val check_move :
    position -> Moves.pol_move -> (Moves.Renaming.t * position) option

  (* trigger_move extends the position by m without type checking it. *)
  val trigger_move : position -> Moves.pol_move -> Moves.Renaming.t * position
end
