module type MOVES = functor (Lang:Language.LANG) ->
  sig
  type player = Proponent | Opponent

  type move

  type action =
  | PDiv
  | Vis of move
    (*| PRecCall of Lang.id *)

  val generate_pmove : Lang.name_type_ctx -> Lang.name -> Lang.interactive_val -> (action * Lang.interactive_env * Lang.name_type_ctx)

  val generate_omove : Lang.name_type_ctx -> (action * Lang.name_type_ctx) list

  val generate_term : Lang.interactive_env -> action -> Lang.computation

  val unify_move : Lang.name Namespan.namespan -> move -> move -> Lang.name Namespan.namespan option

  val synch_move : Lang.name Namespan.namespan -> move -> move -> Lang.name Namespan.namespan option

  val unify_action : Lang.name Namespan.namespan -> action -> action -> Lang.name Namespan.namespan option
  
  val synch_action : Lang.name Namespan.namespan -> action -> action -> Lang.name Namespan.namespan option

  val string_of_action : action -> string
end

module Moves : MOVES