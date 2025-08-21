module type NAMES = sig
  type name
  val string_of_name : name -> string
  val pp_name : Format.formatter -> name -> unit

  val is_callable : name -> bool
  val is_cname : name -> bool

end

module type NAMES_GEN = sig
include NAMES
  val fresh_name : unit -> name
end

