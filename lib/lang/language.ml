module type BASIC = sig
  include Names.NAME

  type term

  val string_of_term : term -> string

  type value

  val string_of_value : value -> string

  type eval_ctx

  val string_of_eval_ctx : eval_ctx -> string
  val fill_hole : eval_ctx -> value -> term
  val apply_value : value -> value -> term
  val get_callback : term -> (name * value * eval_ctx) option
  val get_value : term -> value option
  val is_error : term -> bool
end

module type TYPED = sig
  include BASIC

  type typ

  type typevar
  type typename

  val string_of_type : typ -> string
  val string_of_typename : typename -> string

  type name_ctx = (name, typ) Util.Pmap.pmap

  val get_input_type : typ -> (typevar list*typ)
  val get_output_type : typ -> typ

  val generate_typename_subst : typevar list -> (typename list*(typevar,typ) Util.Pmap.pmap)
  val apply_type_subst : typ -> (typevar,typ) Util.Pmap.pmap -> typ
end

module type WITHNUP = sig
  include TYPED

  module Nup :
    Nup.NUP with type name = name and type value = value and type typ = typ
end
