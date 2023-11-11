module type ACTIONS = sig
  (* To be instantiated *)
  module Moves : Moves.MOVES

  (* *)
  type action = PDiv | PError | Vis of Moves.move (* We might make this type abstract in the future *)

  val get_move_from_action : action -> Moves.move option
  val string_of_action : action -> string
  val inject_move : Moves.move -> action
  val diverging_action : action
  val error_action : action
end
