module type NAME = sig
  type name

  val string_of_name : name -> string
  val is_callable : name -> bool
end

module type CONT_NAMES = sig
  include NAME

  type cont_name

  val inj_cont_name : cont_name -> name
  val get_cont_name : name -> cont_name option
  val string_of_cont_name : cont_name -> string
  val fresh_cname : unit -> cont_name
end