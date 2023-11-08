module type INT = sig
  (* To be instantiated *)
  module OpLang : Lang.Language.LANG
  module Actions : Actions.ACTIONS with type Moves.name = OpLang.name

  (* *)
  open Actions
  open Actions.Moves

  val generate_output_action :
    OpLang.name_type_ctx ->
    OpLang.name ->
    OpLang.interactive_val ->
    action * OpLang.interactive_env * OpLang.name_type_ctx

  val generate_input_moves :
    OpLang.name_type_ctx -> (move * OpLang.name_type_ctx) list

  val check_input_move :
    OpLang.name_type_ctx -> move -> OpLang.name_type_ctx option

  val trigger_computation : OpLang.interactive_env -> move -> OpLang.computation

  val unify_action :
    OpLang.name Util.Namespan.namespan ->
    action ->
    action ->
    OpLang.name Util.Namespan.namespan option

  val synch_move :
    OpLang.name Util.Namespan.namespan ->
    move ->
    move ->
    OpLang.name Util.Namespan.namespan option
end

(*
module type INT = sig
  module Moves : Moves.MOVES
  module Actions : ACTIONS with type move = Moves.move
end
*)

module type INT_F = functor
  (OpLang : Lang.Language.LANG)
  (Moves : Moves.MOVES with type name = OpLang.name)
  -> sig
  include INT with module OpLang = OpLang and module Actions.Moves = Moves
end

(*
module type CPSINT_F = functor (OpLang:Language.CONTLANG) -> sig 
  include CPSINT with module OpLang = OpLang
end
*)
