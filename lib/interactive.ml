module type ACTIONS = sig
  (* To be instantiated *)
  type move
  module Lang : Lang.Language.LANG
  (* *)
  type action
  val get_move_from_action : action -> move option
  val inject_move : move -> action
  val diverging_action : action
  (*=
  | PDiv
  | Vis of Moves.move*)

  val generate_output_action : Lang.name_type_ctx -> Lang.name -> Lang.interactive_val -> (action * Lang.interactive_env * Lang.name_type_ctx)

  val generate_input_moves : Lang.name_type_ctx -> (move * Lang.name_type_ctx) list
  
  val check_input_move : Lang.name_type_ctx -> move -> Lang.name_type_ctx option

  val generate_computation : Lang.interactive_env -> move -> Lang.computation
  
  val unify_action : Lang.name Util.Namespan.namespan -> action -> action -> Lang.name Util.Namespan.namespan option
  
  val synch_move : Lang.name Util.Namespan.namespan -> move -> move -> Lang.name Util.Namespan.namespan option

  val string_of_action : action -> string
end

module type INT = sig
  module Moves : Moves.MOVES
  module Actions : ACTIONS with type move = Moves.move
end

module type INT_F = functor (Lang:Lang.Language.LANG) (Moves:Moves.MOVES) -> sig 
  include INT with module Actions.Lang = Lang and module Moves = Moves
end


(*
module type CPSINT_F = functor (Lang:Language.CONTLANG) -> sig 
  include CPSINT with module Lang = Lang
end
*)