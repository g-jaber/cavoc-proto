module Make
    (Languages : Lang.Definability.WITH_DIRECT_STYLE)
    (DirectMoves :
      Lts.Moves.POLMOVES
        with type copattern =
          Languages.Direct.abstract_normal_form
          * Languages.Direct.IEnv.Renaming.Namectx.t)
    (CpsTypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = Languages.Cps.Renaming
         and type Moves.copattern =
          Languages.Cps.Definability.abstract_normal_form
          * Languages.Cps.Renaming.Namectx.t) : sig
  val cps_play :
    CpsTypingLTS.position ->
    DirectMoves.pol_move list ->
    CpsTypingLTS.Moves.pol_move list end = struct
  module CpsMoves = CpsTypingLTS.Moves
  module Names = CpsMoves.Renaming.Namectx.Names

  let cps_play position moves =
    let step (position, pending, cps_moves)
        ((direction, move) : DirectMoves.pol_move) =
      let cps_move =
        Languages.move_of_direct_style ~pending:(List.nth_opt pending 0) move
      in
      let cps_direction =
        match direction with
        | DirectMoves.Input -> CpsMoves.Input
        | DirectMoves.Output -> CpsMoves.Output in
      let cps_pol_move = (cps_direction, cps_move) in
      match CpsTypingLTS.check_move position cps_pol_move with
      | None ->
          failwith
            "Definability: a direct-style move has no well-typed CPS form. \
             Please report."
      | Some (weakening, target) ->
          let pending =
            if Names.is_cname (CpsMoves.get_subject_name cps_move) then
              List.tl pending
            else
              List.filter Names.is_cname
                (CpsMoves.fresh_names weakening cps_move)
              @ pending in
          (target, pending, cps_pol_move :: cps_moves) in
    let (_, _, cps_moves) = List.fold_left step (position, [], []) moves in
    List.rev cps_moves
end
