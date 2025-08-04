module type LTS = sig
  (* The following field is to be instantiated *)
  module M : Util.Monad.BRANCH
  module EvalMonad : Util.Monad.RUNNABLE

  (* *)
  module Actions : Actions.ACTIONS

  type active_conf
  type passive_conf
  type conf = Active of active_conf | Passive of passive_conf

  val passive_conf_to_yojson : passive_conf -> Yojson.Safe.t
  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val equiv_act_conf : active_conf -> active_conf -> bool

  (* The Proponent transition function return None when an error or diverging action is performed*)
  val p_trans : active_conf -> (Actions.Moves.move * passive_conf) EvalMonad.m
  val o_trans : passive_conf -> Actions.Moves.move -> active_conf option
  val o_trans_gen : passive_conf -> (Actions.Moves.move * active_conf) M.m
end

module type INT_LTS = sig
  module Int : Interactive.INT
  include LTS with module Actions = Int.Actions

  (* init_aconf creates an configuration from a computation and a name context for Opponent. 
     Its store, interactive env, and name context for Proponent are all set to empty*)
  val init_aconf :
    Int.IntLang.computation -> Int.IntLang.name_ctx -> active_conf

  (* init_pconf creates a passive configuration from a store, an interactive env, 
     a name context for Proponent and a name context for Opponent. *)
  val init_pconf :
    Int.IntLang.Store.store ->
    Int.IntLang.interactive_env ->
    Int.IntLang.name_ctx ->
    Int.IntLang.name_ctx ->
    passive_conf

  val extract_interactive_ctx : conf -> Int.interactive_ctx
end

module type INT_LTS_F = functor (Int : Interactive.INT) ->
  INT_LTS with module Int = Int and module M = Int.IntLang.M
