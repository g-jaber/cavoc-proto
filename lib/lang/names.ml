module type NAMES = sig
  type name
  val string_of_name : name -> string
  val pp_name : Format.formatter -> name -> unit

  val is_fname : name -> bool
  val is_cname : name -> bool
  val fresh_cname : unit -> name
end