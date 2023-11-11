module type HISLTS = sig
  type move
  type active_conf
  type passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val p_trans : active_conf -> move -> passive_conf
  val o_trans_check : passive_conf -> move -> active_conf option
end

module type HISLTS_INIT = sig
  include HISLTS

  type name

  val init_aconf : name list -> active_conf
  val init_pconf : name list -> name list -> passive_conf
end

module type HISLTS_INIT_F = functor (Int : Interactive.INT) -> sig
  include
    HISLTS_INIT
      with type move = Int.Actions.Moves.move
       and type name = Int.IntLang.name
end
