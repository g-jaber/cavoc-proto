(* A view-function strategy as a Strategy.LTS over the typing LTS of the
   language it was built over, directly composable against an ordinary
   component of that language. *)
module MakeComponent
    (IntLang : Lang.Interactive.LANG)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = IntLang.IEnv.Renaming
         and module BranchMonad = IntLang.BranchMonad
         and type Moves.copattern =
          IntLang.abstract_normal_form * IntLang.IEnv.Renaming.Namectx.t
         and type store_ctx = IntLang.Storectx.t)
    (ExtraMemory :
      Lts.Extra_memory.EXTRA_MEMORY
        with type move = TypingLTS.Moves.move
         and type name = TypingLTS.Moves.Renaming.Namectx.Names.name
         and type namectx = TypingLTS.Moves.Renaming.Namectx.t
         and type renaming = TypingLTS.Moves.Renaming.t) =
struct
  module ViewFunction =
    Lts.View_function.Make (IntLang) (TypingLTS) (ExtraMemory)

  module ViewFunctionLang =
    Lts.View_function.MakeLang (IntLang) (ViewFunction) (IntLang.EvalMonad)

  include Ogslts.Make (ViewFunctionLang) (TypingLTS)

  (* Each initial Player name is bound to its empty-view pointed view, over
     a domain rebuilt with empty hints. *)
  let initial_ienv position =
    let namectxP = TypingLTS.get_namectxP position in
    let namectxO = TypingLTS.get_namectxO position in
    List.fold_left
      (fun ienv name ->
        let typ = TypingLTS.Moves.Renaming.Namectx.lookup_exn namectxP name in
        snd
          (ViewFunctionLang.IEnv.add_fresh ienv "" typ
             (ViewFunction.initial_pointed_view position name)))
      (ViewFunctionLang.IEnv.empty namectxO)
      (TypingLTS.Moves.Renaming.Namectx.get_names namectxP)

  let initial_passive_conf strategy position =
    init_pconf
      (ViewFunctionLang.initial_store strategy position)
      (initial_ienv position)
      (TypingLTS.get_namectxP position)
      (TypingLTS.get_namectxO position)

  (* The view function stands at the position after the initial move, as the
     builder does for the tail of a play starting with it. *)
  let initial_active_conf strategy
      ((direction, initial_move) as initial_action : TypingLTS.Moves.pol_move)
      position =
    assert (direction = TypingLTS.Moves.Output);
    assert (
      TypingLTS.get_namectxP position = TypingLTS.Moves.Renaming.Namectx.empty);
    let (_, position_after_initial_move) =
      TypingLTS.trigger_move position initial_action in
    let store =
      ViewFunctionLang.initial_store strategy position_after_initial_move in
    init_aconf
      (ViewFunctionLang.initial_move_opconf initial_move store)
      (TypingLTS.get_namectxO position)
end
