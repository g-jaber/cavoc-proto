module type NAMECTX = sig
  type name

  type name_ctx

  val empty_name_ctx : name_ctx
  val concat_name_ctx : name_ctx -> name_ctx -> name_ctx
  val string_of_name_ctx : name_ctx -> string
  val pp_name_ctx : Format.formatter -> name_ctx -> unit
  val get_names_from_name_ctx : name_ctx -> name list
end
