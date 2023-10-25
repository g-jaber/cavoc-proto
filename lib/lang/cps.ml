module type CONT_NAMES = sig
  (* to be instantiated *)
  type name
  (* *)
  type cont_name
  val inj_cont_name : cont_name -> name
  val get_cont_name : name -> cont_name option
  val string_of_cont_name : cont_name -> string
end

module type LANG = sig
  include Language.LANG
  include CONT_NAMES with type name := name
end