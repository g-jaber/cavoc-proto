module type ACTIONS = sig
  (* To be instantiated *)
  module Moves : Moves.MOVES

  (* *)
  type action

  val get_move_from_action : action -> Moves.move option
  val string_of_action : action -> string
  val inject_move : Moves.move -> action
  val diverging_action : action
end
