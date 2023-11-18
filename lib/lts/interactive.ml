module type INT = sig
  (* To be instantiated *)
  module IntLang : Lang.Interactive.LANG

  module Actions :
    Actions.ACTIONS
      with type Moves.name = IntLang.name
       and type Moves.kind = IntLang.kind_interact
  (* *)

  val generate_output_move :
    IntLang.name_type_ctx ->
    IntLang.kind_interact ->
    IntLang.glue_val ->
    Actions.Moves.move * IntLang.interactive_env * IntLang.name_type_ctx

  val generate_input_moves :
    IntLang.name_type_ctx ->
    (Actions.Moves.move * IntLang.name_type_ctx) IntLang.M.m

  val check_input_move :
    IntLang.name_type_ctx ->
    IntLang.name_type_ctx ->
    Actions.Moves.move ->
    IntLang.name_type_ctx option

  val trigger_computation :
    IntLang.interactive_env -> Actions.Moves.move -> IntLang.computation
end

module type INT_F = functor
  (IntLang : Lang.Interactive.LANG)
  (Moves : Moves.MOVES
             with type name = IntLang.name
              and type kind = IntLang.kind_interact)
  -> sig
  include INT with module IntLang = IntLang and module Actions.Moves = Moves
end
