(* The store part of a move: the content of every public location, over the
   disclosed store context extended by the locations the move discloses. *)
module type DISCLOSED_STORE = sig
  type store_ctx
  type t [@@deriving to_yojson]

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val empty : t

  (* The heap contents are compared only when asked: POGS relates heaps a
     posteriori. *)
  val is_equiv : compare_heaps:bool -> t -> t -> bool
end
