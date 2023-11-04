module type LTS =
  sig
    (* The following field is to be instantiated *)
    module M:Util.Monad.BRANCH
    (* *)
    module Actions: Actions.ACTIONS
    type active_conf
    type passive_conf
    type conf = 
      | Active of active_conf
      | Passive of passive_conf
    val string_of_active_conf : active_conf -> string
    val string_of_passive_conf : passive_conf -> string
    val equiv_aconf : active_conf -> active_conf -> bool
    val p_trans : active_conf -> Actions.action * passive_conf option
    val o_trans : passive_conf -> Actions.Moves.move -> active_conf option
    val o_trans_gen : passive_conf -> (Actions.Moves.move * active_conf) M.m
end

module type INT_LTS = sig
  module Int : Interactive.INT
  include LTS with module Actions = Int.Actions
  val init_aconf : Int.OpLang.computation -> Int.OpLang.name_type_ctx -> active_conf
  val init_pconf : Int.OpLang.interactive_env -> Int.OpLang.name_type_ctx -> Int.OpLang.name_type_ctx -> passive_conf
end

module type INT_LTS_F = functor (M:Util.Monad.BRANCH) (Int:Interactive.INT) -> INT_LTS with module Int = Int and module M = M