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

  (* generate_move Γₓ return
      all the pairs (m,Γₓ') such that
      there exists a name context Δ for the free names of m such that
      Γₓ ⊢ m ▷ Δ  and Γₓ' = Γₓ + Δ.
     It uses the branching monad from BranchMonad to do so. *)
  val generate_moves : position -> (Moves.pol_move * position) BranchMonad.m

  (* The move weakened into the context the direction selects at the
     position, the LTS's own choice of where its fresh names go; the identity
     when moves carry no weakening. *)
  (* Only the domain of the carried map is read, so a move weakened into
     another context is weakened anew. *)
  val weaken_move : position -> Moves.direction -> Moves.move -> Moves.move

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed, or when the weakening it
     carries is not the one weaken_move gives. *)
  val check_move : position -> Moves.pol_move -> position option

  (* trigger_move trusts the weakening carried by the move: the extended
     side-context is taken from its image. *)
  val trigger_move : position -> Moves.pol_move -> position
end
