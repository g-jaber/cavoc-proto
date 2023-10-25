module type MOVES = sig

  (* to be instantiated *)
  type kind
  type data
  type player = Proponent | Opponent
  (* *)

  type move
  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_player : move -> player
  val dual : move -> move
end

module type MOVESF = functor (Lang:Lang.Language.LANG) -> 
  sig include MOVES with
    type kind = Lang.name
    and type data = Lang.nup
  end



