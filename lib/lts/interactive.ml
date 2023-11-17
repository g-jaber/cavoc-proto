module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG
  module Actions : Actions.ACTIONS with type Moves.name = IntLang.name
  (* *)

  val generate_output_action :
    IntLang.name_type_ctx ->
    IntLang.name ->
    IntLang.glue_val ->
    Actions.action
    * IntLang.interactive_env
    * IntLang.name_type_ctx

  val generate_input_moves :
    IntLang.name_type_ctx ->
    (Actions.Moves.move * IntLang.name_type_ctx) list

  val check_input_move :
    IntLang.name_type_ctx ->
    IntLang.name_type_ctx ->
    Actions.Moves.move ->
    IntLang.name_type_ctx option

  val trigger_computation :
    IntLang.interactive_env ->
    Actions.Moves.move ->
    IntLang.computation

  val unify_action :
    IntLang.name Util.Namespan.namespan ->
    Actions.action ->
    Actions.action ->
    IntLang.name Util.Namespan.namespan option

  val synch_move :
    IntLang.name Util.Namespan.namespan ->
    Actions.Moves.move ->
    Actions.Moves.move ->
    IntLang.name Util.Namespan.namespan option
end

(*
module type INT = sig
  module Moves : Moves.MOVES
  module Actions : ACTIONS with type move = Moves.move
end
*)

module type INT_F = functor
  (IntLang : Lang.Interactive.LANG)
  (Moves : Moves.MOVES with type name = IntLang.name)
  -> sig
  include INT with module IntLang = IntLang and module Actions.Moves = Moves
end

(*
module type CPSINT_F = functor (IntLang:Language.CONTLANG) -> sig 
  include CPSINT with module IntLang = IntLang
end
*)
