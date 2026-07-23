module type LTS = sig
  module Moves : Moves.POLMOVES
  module BranchMonad : Util.Monad.BRANCH

  type store_ctx

  (* *)

  type position [@@deriving to_yojson]

  val get_namectxO : position -> Moves.Renaming.Namectx.t
  val get_namectxP : position -> Moves.Renaming.Namectx.t
  val get_storectx : position -> store_ctx
  val init_act_pos : store_ctx -> Moves.Renaming.Namectx.t -> Moves.Renaming.Namectx.t -> position
  val init_pas_pos : store_ctx -> Moves.Renaming.Namectx.t -> Moves.Renaming.Namectx.t -> position
  val string_of_position : position -> string
  val pp_position : Format.formatter -> position -> unit

  (* generate_move Γₓ return
      all the pairs (m,Γₓ') such that
      there exists a name context Δ for the free names of m such that
      Γₓ ⊢ m ▷ Δ  and Γₓ' = Γₓ + Δ.
     It uses the branching monad from BranchMonad to do so. *)
  val generate_moves : position -> (Moves.pol_move * position) BranchMonad.m

  (* place pos dir nn Δ returns the placement weakening w : Δ ↪ Γ',
     where Γ' extends the ambient context of pos selected by dir
     (namectxP for Output, namectxO for Input) according to the LTS's
     placement policy. The subject nn is unused by the standard instances
     (placement depends only on the direction) but lets instances place
     per move. place is pure policy: it does not advance the position. *)
  val place :
    position ->
    Moves.direction ->
    Moves.Renaming.Namectx.Names.name ->
    Moves.Renaming.Namectx.t ->
    Moves.Renaming.t

  (* check_move Γₓ m return Some Δ
     when there exists a name context Γ for the free names of m such that
      Γₓ ⊢ m ▷ Δ.
     It returns None when m is not well-typed.
     The placement carried by m is validated against the policy of place;
     a mis-placed move is rejected. *)
  val check_move : position -> Moves.pol_move -> position option

  (* trigger_move trusts the placement carried by the move: the extended
     side-context is taken from its image. *)
  val trigger_move : position -> Moves.pol_move -> position
end
