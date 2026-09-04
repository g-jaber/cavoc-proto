(* The sequential composition of a client over G with the module it is
   written against, presented under G'.
   It is defined using the functorial-ogs approach: the join of the parallel composition
   of the two splits. *)
module Make
    (G : Typing.LTS)
    (G' : Typing.LTS with module Moves = G.Moves)
    (S1 : Strategy.LTS with module TypingLTS.Moves = G.Moves)
    (S2 :
      Strategy.LTS
        with module TypingLTS.Moves = G.Moves
         and module EvalMonad = S1.EvalMonad) : sig
  include
    Strategy.LTS with module TypingLTS = G' and module EvalMonad = S1.EvalMonad

  type renaming = Typing.Polarized_renaming(G.Moves).t

  type comp_move =
    | ExternalMove of G'.Moves.pol_move
    | SyncMove of G.Moves.pol_move

  (* The identity from the concatenation of the two sides of a pair. *)
  val identity_renaming : G.position * G.position -> renaming

  (* The client over (client_position, shared_position), the module over
     (shared_position, module_position). *)
  val initial_pconf :
    G'.store_ctx ->
    client_pconf:S1.passive_conf ->
    client_position:G.position ->
    client_renaming:renaming ->
    shared_position:G.position ->
    module_pconf:S2.passive_conf ->
    module_position:G.position ->
    module_renaming:renaming ->
    passive_conf

  val string_of_comp_move : comp_move -> string
  val string_of_comp_move_from : active_conf -> comp_move -> string
  val par_p_trans : active_conf -> (comp_move * conf) EvalMonad.m
end = struct
  module SplitClient = Strategy.Split (G) (S1)
  module SplitModule = Strategy.Split (G) (S2)

  module Parallel =
    Parallel_compose.Make (G) (G) (G) (SplitClient) (SplitModule)

  module Joined = Strategy.Join (G) (G') (Parallel)
  include Joined
  module Concatenation = Typing.Concatenation (G)
  module Renaming = Concatenation.Polarized_renaming

  type comp_move =
    | ExternalMove of G'.Moves.pol_move
    | SyncMove of G.Moves.pol_move

  let identity_renaming = SplitClient.identity_renaming

  let initial_pconf storectx ~client_pconf ~client_position ~client_renaming
      ~shared_position ~module_pconf ~module_position ~module_renaming =
    Joined.join_pconf storectx
      ( SplitClient.split_pconf client_pconf
          (client_position, shared_position)
          client_renaming,
        SplitModule.split_pconf module_pconf
          (shared_position, module_position)
          module_renaming )

  let string_of_comp_move = function
    | ExternalMove move -> G'.Moves.string_of_pol_move move
    | SyncMove move -> "τ[" ^ G.Moves.string_of_pol_move move ^ "]"

  let string_of_comp_move_from aconf = function
    | ExternalMove move ->
        let position = get_active_pos aconf in
        let show_name =
          G'.Moves.Renaming.Namectx.show_name_in (G'.get_namectxO position)
        in
        let (weakening, _) = G'.trigger_move position move in
        G'.Moves.string_of_pol_move_in ~show_name weakening move
    | SyncMove move ->
        Parallel.string_of_comp_move_from aconf.act (Parallel.SyncMove move)

  (* One step of the parallel composition, joined. *)
  let par_p_trans aconf =
    let open EvalMonad in
    let* (cm, next) = Parallel.par_p_trans aconf.act in
    match (cm, next) with
    | (Parallel.ExternalMove ((dir, _) as action), Parallel.Passive pas) ->
        let position = Parallel.get_active_pos aconf.act in
        let own_action =
          Renaming.rename_move aconf.act_renaming
            (Concatenation.join_move position action) in
        let (_, pas_position) = G'.trigger_move aconf.act_position own_action in
        return
          ( ExternalMove own_action,
            Passive
              {
                pas;
                pas_position;
                pas_renaming=
                  Concatenation.extend_renaming (dir = G.Moves.Input) position
                    action aconf.act_renaming;
              } )
    | (Parallel.SyncMove sync, Parallel.Active act) ->
        return (SyncMove sync, Active { aconf with act })
    | (Parallel.ExternalMove _, Parallel.Active _)
    | (Parallel.SyncMove _, Parallel.Passive _) ->
        assert false
end
