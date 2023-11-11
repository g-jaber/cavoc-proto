module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG
  module Actions : Actions.ACTIONS with type Moves.name = IntLang.name

  (* *)

  val generate_output_action :
    IntLang.Focusing.name_type_ctx ->
    IntLang.name ->
    IntLang.Focusing.glue_val ->
    Actions.action
    * IntLang.Focusing.interactive_env
    * IntLang.Focusing.name_type_ctx

  val generate_input_moves :
    IntLang.Focusing.name_type_ctx ->
    (Actions.Moves.move * IntLang.Focusing.name_type_ctx) list

  val check_input_move :
    IntLang.Focusing.name_type_ctx ->
    IntLang.Focusing.name_type_ctx ->
    Actions.Moves.move ->
    IntLang.Focusing.name_type_ctx option

  val trigger_computation :
    IntLang.Focusing.interactive_env ->
    Actions.Moves.move ->
    IntLang.Focusing.computation

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
