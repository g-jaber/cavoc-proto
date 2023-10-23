module type BILTS = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) ->
  sig
    type active_conf
    type passive_conf
    val string_of_active_conf : active_conf -> string
    val string_of_passive_conf : passive_conf -> string
    val p_trans : active_conf -> Moves.Moves(Lang).action * passive_conf option
    val init_aconf : Lang.computation -> Lang.name_type_ctx -> active_conf
    val init_pconf : Lang.interactive_env -> Lang.name_type_ctx -> Lang.name_type_ctx -> passive_conf
    val equiv_aconf : active_conf -> active_conf -> bool
    val o_trans : passive_conf -> (Moves.Moves(Lang).action * active_conf) M.m
end

(*
module type FBILTS = functor (Lang:Language.LANG) (M:Monad.LISTMONAD) ->
  sig
  include module type of BILTS(Lang)
  val o_trans : passive_conf -> (Moves.Moves(Lang).action * active_conf) M.m
end
*)