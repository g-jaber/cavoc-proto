module type MOVES = sig
  (* to be instantiated *)
  type kind
  type data
  type name
  (* *)

  type direction = Input | Output | None
  type move

  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_direction : move -> direction
  val switch_direction : move -> move
  val get_subject_names : move -> name list
  val get_transmitted_names : move -> name list
end

module type MOVESF = functor (Lang : Lang.Language.WITHNUP) -> sig
  include MOVES with type kind = Lang.name and type data = Lang.Nup.nup
end
