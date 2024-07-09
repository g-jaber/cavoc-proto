module type NAME = sig
  type name

  val string_of_name : name -> string
  val pp_name : Format.formatter -> name -> unit
  val is_callable : name -> bool

  val is_fname : name -> bool
  val is_cname : name -> bool
end

module type CONT_NAMES = sig
  include NAME

  type cont_name

  val inj_cont_name : cont_name -> name
  val get_cont_name : name -> cont_name option
  val string_of_cont_name : cont_name -> string
  val pp_cont_name : Format.formatter -> cont_name -> unit
  val fresh_cname : unit -> cont_name
end