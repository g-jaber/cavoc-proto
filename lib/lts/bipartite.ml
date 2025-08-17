module type LTS = sig
  (* The following field is to be instantiated *)
  module OBranchingMonad : Util.Monad.BRANCH
  module EvalMonad : Util.Monad.RUNNABLE

  (* *)
  module Moves : Moves.MOVES

  type active_conf
  type passive_conf
  type conf = Active of active_conf | Passive of passive_conf

  val passive_conf_to_yojson : passive_conf -> Yojson.Safe.t
  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool

  val p_trans : active_conf -> (Moves.move * passive_conf) EvalMonad.m
  val o_trans : passive_conf -> Moves.move -> active_conf option
  val o_trans_gen : passive_conf -> (Moves.move * active_conf) OBranchingMonad.m
end

module type INT_LTS = sig
  module Int : Interactive.INT
  include LTS with module Moves = Int.GameLTS.Moves

  (* init_aconf creates an configuration from an operational configuration and a name context for Opponent. 
     Its interactive env, and name context for Proponent are all set to empty*)
  val init_aconf :
    Int.IntLang.opconf -> Int.IntLang.name_ctx -> active_conf

  (* init_pconf creates a passive configuration from a store, an interactive env, 
     a name context for Proponent and a name context for Opponent. *)
  val init_pconf :
    Int.IntLang.store ->
    Int.IntLang.interactive_env ->
    Int.IntLang.name_ctx ->
    Int.IntLang.name_ctx ->
    passive_conf
end

module type INT_LTS_F = functor (Int : Interactive.INT) ->
  INT_LTS with module Int = Int and module OBranchingMonad = Int.GameLTS.BranchMonad
