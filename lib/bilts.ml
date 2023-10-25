module type LTS =
  sig
    (* The following field is to be instantiated *)
    module M:Monad.LISTMONAD
    (* *)
    type action
    type move
    val get_move_from_action : action -> move option
    type active_conf
    type passive_conf
    type conf = 
      | Active of active_conf
      | Passive of passive_conf
    val string_of_active_conf : active_conf -> string
    val string_of_passive_conf : passive_conf -> string
    val p_trans : active_conf -> action * passive_conf option
    val equiv_aconf : active_conf -> active_conf -> bool
    val o_trans : passive_conf -> (action * active_conf) M.m
end

module type INT_LTS = sig
  module Int : Interactive.INT
  include LTS with type action = Int.Actions.action and type move = Int.Moves.move
  val init_aconf : Int.Actions.Lang.computation -> Int.Actions.Lang.name_type_ctx -> active_conf
  val init_pconf : Int.Actions.Lang.interactive_env -> Int.Actions.Lang.name_type_ctx -> Int.Actions.Lang.name_type_ctx -> passive_conf
end

module type INT_LTS_F = functor (M:Monad.LISTMONAD) (Int:Interactive.INT) -> INT_LTS with module Int = Int and module M = M