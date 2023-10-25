module type CONT_NAMES = sig
  (* to be instantiated *)
  type name
  (* *)
  type cont_name
  val inj_cont_name : cont_name -> name
  val get_cont_name : name -> cont_name option
  val string_of_cont_name : cont_name -> string
end

module type LANG = sig
  include Language.LANG
  include CONT_NAMES with type name := name
end

module type MOVES = sig
  (* to be instantiated *)
  type kind
  type data
  type player = Proponent | Opponent
  (* *)

  type move = player * kind * data
  val string_of_move : move -> string
  val get_kind : move -> kind
  val get_data : move -> data
  val get_player : move -> player
  val dual : move -> move
  module ContNames : CONT_NAMES
  val get_transmitted_continuation_names : move -> ContNames.cont_name list
  val get_active_continuation_name : move -> ContNames.cont_name option
end

module Moves_Make : functor (_ : LANG) -> MOVES

module type INT = sig
  module Moves : MOVES
  module Actions : Interactive.ACTIONS with type move = Moves.move and type Lang.name = Moves.ContNames.name
end

module Int_Make : functor (Lang : LANG) -> INT with type Moves.ContNames.name = Lang.name