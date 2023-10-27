module type MOVES = sig
  (* to be instantiated *)
  type kind
  type data
  type direction =  Input | Output | None
  (* *)

  type move = direction * kind * data
  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_player : move -> direction
  val switch_direction : move -> move
  module ContNames : Lang.Cps.CONT_NAMES
  val get_transmitted_continuation_names : move -> ContNames.cont_name list
  val get_active_continuation_name : move -> ContNames.cont_name option
end

module Moves_Make : functor (_ : Lang.Cps.LANG) -> MOVES

module type INT = sig
  module Moves : MOVES
  module Actions : Interactive.ACTIONS with type move = Moves.move and type Lang.name = Moves.ContNames.name
end

module Int_Make : functor (Lang : Lang.Cps.LANG) -> INT with type Moves.ContNames.name = Lang.name