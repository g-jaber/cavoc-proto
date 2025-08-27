module type LTS = sig
  (* The following field is to be instantiated *)
  module OBranchingMonad : Util.Monad.BRANCH
  module EvalMonad : Util.Monad.RUNNABLE

  (* *)
  module Moves : Moves.POLMOVES
  type name_ctx
  val get_names : name_ctx -> Moves.Names.name list

  type active_conf
  type passive_conf [@@deriving to_yojson]
  type conf = Active of active_conf | Passive of passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool

  val p_trans : active_conf -> ((Moves.pol_move*name_ctx) * passive_conf) EvalMonad.m
  val o_trans : passive_conf -> (Moves.pol_move*name_ctx) -> active_conf option
  val o_trans_gen : passive_conf -> ((Moves.pol_move*name_ctx) * active_conf) OBranchingMonad.m
end

module type INT_LTS = sig
  (* To be instantiated *)
  type opconf
  type store
  type interactive_env
  (* *)
  include LTS

  (* init_aconf creates an configuration from an operational configuration and a name context for Opponent. 
     Its interactive env, and name context for Proponent are all set to empty*)
  val init_aconf :
    opconf -> name_ctx -> active_conf

  (* init_pconf creates a passive configuration from a store, an interactive env, 
     a name context for Proponent and a name context for Opponent. *)
  val init_pconf :
    store ->
    interactive_env ->
    name_ctx ->
    name_ctx ->
    passive_conf
end
