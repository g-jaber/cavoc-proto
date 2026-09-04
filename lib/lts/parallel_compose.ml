(* The parallel composition of a strategy over G1 ⊸ G2 with a strategy over
   G2 ⊸ G3 into a strategy over G1 ⊸ G3. *)
module Make
    (G1 : Typing.LTS)
    (G2 : Typing.LTS with module BranchMonad = G1.BranchMonad)
    (G3 : Typing.LTS with module BranchMonad = G1.BranchMonad)
    (S1 : Strategy.LTS with module TypingLTS = Typing.Lollipop(G1)(G2))
    (S2 :
      Strategy.LTS
        with module TypingLTS = Typing.Lollipop(G2)(G3)
         and module EvalMonad = S1.EvalMonad) : sig
  (* The configurations are those of the tensor of the two strategies. *)
  type active_conf =
    | ActiveLeft of S1.active_conf * S2.passive_conf
    | ActiveRight of S1.passive_conf * S2.active_conf

  type passive_conf = S1.passive_conf * S2.passive_conf

  include
    Strategy.LTS
      with module TypingLTS = Typing.Lollipop(G1)(G3)
       and module EvalMonad = S1.EvalMonad
       and type active_conf := active_conf
       and type passive_conf := passive_conf

  type comp_move =
    | ExternalMove of TypingLTS.Moves.pol_move
    | SyncMove of G2.Moves.pol_move

  val string_of_comp_move : comp_move -> string
  val string_of_comp_move_from : active_conf -> comp_move -> string
  val par_p_trans : active_conf -> (comp_move * conf) EvalMonad.m
end = struct
  module Tensor = Strategy.Tensor (S1) (S2)
  module TypingLTS = Typing.Lollipop (G1) (G3)
  module EvalMonad = S1.EvalMonad

  type active_conf = Tensor.active_conf =
    | ActiveLeft of S1.active_conf * S2.passive_conf
    | ActiveRight of S1.passive_conf * S2.active_conf

  type passive_conf = S1.passive_conf * S2.passive_conf
  type conf = Active of active_conf | Passive of passive_conf

  type comp_move =
    | ExternalMove of TypingLTS.Moves.pol_move
    | SyncMove of G2.Moves.pol_move

  let passive_conf_to_yojson = Tensor.passive_conf_to_yojson
  let pp_active_conf = Tensor.pp_active_conf
  let pp_passive_conf = Tensor.pp_passive_conf
  let string_of_active_conf = Tensor.string_of_active_conf
  let string_of_passive_conf = Tensor.string_of_passive_conf
  let equiv_act_conf = Tensor.equiv_act_conf

  (* The composite's position and G2's, read off the tensor's. *)
  let composite_position ((pos1, _), (_, pos3)) = (pos1, pos3)
  let middle_position ((_, pos2), _) = pos2
  let get_active_pos aconf = composite_position (Tensor.get_active_pos aconf)
  let get_passive_pos pconf = composite_position (Tensor.get_passive_pos pconf)

  let string_of_comp_move = function
    | ExternalMove move -> TypingLTS.Moves.string_of_pol_move move
    | SyncMove move -> "τ[" ^ G2.Moves.string_of_pol_move move ^ "]"

  let string_of_comp_move_from aconf = function
    | ExternalMove move ->
        let position = get_active_pos aconf in
        let show_name =
          TypingLTS.Moves.Renaming.Namectx.show_name_in
            (TypingLTS.get_namectxO position) in
        let (weakening, _) = TypingLTS.trigger_move position move in
        TypingLTS.Moves.string_of_pol_move_in ~show_name weakening move
    | SyncMove ((dir, move) as sync) ->
        let position = middle_position (Tensor.get_active_pos aconf) in
        let show_name =
          G2.Moves.Renaming.Namectx.show_name_in
            (match dir with
            | G2.Moves.Input -> G2.get_namectxP position
            | G2.Moves.Output -> G2.get_namectxO position) in
        let (weakening, _) = G2.trigger_move position sync in
        G2.Moves.string_of_move_in ~show_name weakening move

  (* A synchronization handed to the other strategy as its Input move in
     G2; both copies of the position of G2 take the same move. *)
  let synchronize sync = function
    | None ->
        failwith
          ("The synchronization move "
          ^ G2.Moves.string_of_pol_move sync
          ^ " was refused by the receiving strategy. Please report.")
    | Some aconf -> (SyncMove sync, Active aconf)

  let par_p_trans aconf =
    let open EvalMonad in
    let* ((dir, move), pconf) = Tensor.p_trans aconf in
    match move with
    | Either.Left (Either.Left move1) ->
        return (ExternalMove (dir, Either.Left move1), Passive pconf)
    | Either.Right (Either.Right move3) ->
        return (ExternalMove (dir, Either.Right move3), Passive pconf)
    | Either.Left (Either.Right move2) ->
        return
          (synchronize (G2.Moves.Input, move2)
             (Tensor.o_trans pconf
                (Tensor.TypingLTS.Moves.Input, Either.Right (Either.Left move2))))
    | Either.Right (Either.Left move2) ->
        return
          (synchronize (G2.Moves.Output, move2)
             (Tensor.o_trans pconf
                (Tensor.TypingLTS.Moves.Input, Either.Left (Either.Right move2))))

  (* Infinite chattering is divergence of the composite. *)
  let rec p_trans aconf =
    let open EvalMonad in
    let* (cm, next) = par_p_trans aconf in
    match (cm, next) with
    | (ExternalMove move, Passive pconf) -> return (move, pconf)
    | (SyncMove _, Active aconf) -> p_trans aconf
    | (ExternalMove _, Active _) | (SyncMove _, Passive _) -> assert false

  let o_trans pconf ((dir, move) : TypingLTS.Moves.pol_move) =
    match move with
    | Either.Left move1 ->
        Tensor.o_trans pconf (dir, Either.Left (Either.Left move1))
    | Either.Right move3 ->
        Tensor.o_trans pconf (dir, Either.Right (Either.Right move3))

  let o_trans_gen pconf =
    let open TypingLTS.BranchMonad in
    let* (move, _, _) =
      TypingLTS.generate_moves (get_passive_pos pconf) TypingLTS.Moves.Input
    in
    match o_trans pconf move with
    | None -> fail ()
    | Some aconf -> return (move, aconf)
end
