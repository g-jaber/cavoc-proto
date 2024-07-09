module type HISLTS = sig
  type move
  type active_conf
  type passive_conf

  val string_of_active_conf : active_conf -> string
  val string_of_passive_conf : passive_conf -> string
  val pp_active_conf : Format.formatter -> active_conf -> unit
  val pp_passive_conf : Format.formatter -> passive_conf -> unit
  val p_trans : active_conf -> move -> passive_conf
  val o_trans_check : passive_conf -> move -> active_conf option
end

module type HISLTS_INIT = sig
  include HISLTS

  type name

  val init_aconf : name list -> active_conf
  val init_pconf : name list -> name list -> passive_conf
end
