module type HISLTS = sig
  type move
  type conf [@@deriving to_yojson]

  val string_of_conf : conf -> string
  val pp_conf : Format.formatter -> conf -> unit
  val trans_check : conf -> move -> conf option
end

module type HISLTS_INIT = sig
  include HISLTS

  type name

  val init_act_conf : name list -> name list -> conf
  val init_pas_conf : name list -> name list -> conf
end
