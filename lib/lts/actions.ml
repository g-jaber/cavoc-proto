module type ACTIONS = sig
  (* To be instantiated *)
  module Moves : Moves.MOVES

  (* *)
  type action =
    | PDiv
    | PError
    | Vis of Moves.move (* We might make this type abstract in the future *)

  val get_move_from_action : action -> Moves.move option
  val pp_action : Format.formatter -> action -> unit
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

module Make (Moves : Moves.MOVES) = struct
  module Moves = Moves

  type action = PDiv | PError | Vis of Moves.move

  let get_move_from_action = function
    | Vis move -> Some move
    | PError | PDiv -> None

  let inject_move move = Vis move
  let diverging_action = PDiv
  let error_action = PError

  let pp_action fmt = function
  | PDiv -> Format.pp_print_string fmt "div"
  | PError -> Format.pp_print_string fmt "error"
  | Vis move -> Moves.pp_move fmt move
  let string_of_action = Format.asprintf "%a" pp_action 
  let unify_action span act1 act2 =
    match (act1, act2) with
    | (Vis move1, Vis move2) -> Moves.unify_move span move1 move2
    | (PDiv, PDiv) -> Some span
    | (PError, PError) -> Some span
    | _ -> None
end
(*
module Make (IntLang : Lang.Interactive.LANG) :
  ACTIONS
    (*with type Moves.name = IntLang.Name.name*)
     (*and type Moves.kdata = IntLang.abstract_normal_form*) = struct
     include Make'(Moves.Make (IntLang))
end
*)