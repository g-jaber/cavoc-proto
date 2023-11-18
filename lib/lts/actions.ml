module type ACTIONS = sig
  (* To be instantiated *)
  module Moves : Moves.MOVES

  (* *)
  type action =
    | PDiv
    | PError
    | Vis of Moves.move (* We might make this type abstract in the future *)

  val get_move_from_action : action -> Moves.move option
  val string_of_action : action -> string
  val inject_move : Moves.move -> action
  val diverging_action : action
  val error_action : action

  val unify_action :
    Moves.name Util.Namespan.namespan ->
    action ->
    action ->
    Moves.name Util.Namespan.namespan option
end

module Make (IntLang : Lang.Interactive.LANG) :
  ACTIONS
    with type Moves.name = IntLang.name
     and type Moves.data = IntLang.abstract_val
     and type Moves.kind = IntLang.kind_interact = struct
  module Moves = Moves.Make (IntLang)

  type action = PDiv | PError | Vis of Moves.move

  let get_move_from_action = function
    | Vis move -> Some move
    | PError | PDiv -> None

  let inject_move move = Vis move
  let diverging_action = PDiv
  let error_action = PError

  let string_of_action = function
    | PDiv -> "Div"
    | PError -> "Error"
    | Vis move -> Moves.string_of_move move

  let unify_action span act1 act2 =
    match (act1, act2) with
    | (Vis move1, Vis move2) -> Moves.unify_move span move1 move2
    | (PDiv, PDiv) -> Some span
    | (PError, PError) -> Some span
    | _ -> None
end
