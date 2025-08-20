module type TYPECTX = sig
  type name
  type t [@@deriving to_yojson]

  val empty : t
  val concat : t -> t -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val get_names : t -> name list
end
