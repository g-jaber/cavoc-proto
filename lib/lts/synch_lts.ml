module Make (IntLts : Strategy.LTS_WITH_INIT) :
  Strategy.LTS_WITH_INIT_BIN
    with type TypingLTS.Moves.Renaming.Namectx.t =
      IntLts.TypingLTS.Moves.Renaming.Namectx.t
     and type 'a EvalMonad.r = 'a IntLts.EvalMonad.r = struct
  module TypingLTS = IntLts.TypingLTS
  module EvalMonad = IntLts.EvalMonad

  type active_conf = IntLts.active_conf * IntLts.active_conf
  type passive_conf = IntLts.passive_conf * IntLts.passive_conf

  let passive_conf_to_yojson (pas_conf1, pas_conf2) =
    `List
      [
        IntLts.passive_conf_to_yojson pas_conf1;
        IntLts.passive_conf_to_yojson pas_conf2;
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (act_conf1, act_conf2) =
    Format.fprintf fmt "@[⟨%a |@, %a⟩]" IntLts.pp_active_conf act_conf1
      IntLts.pp_active_conf act_conf2

  let pp_passive_conf fmt (pas_conf1, pas_conf2) =
    Format.fprintf fmt "@[⟨%a |@, %a⟩]" IntLts.pp_passive_conf pas_conf1
      IntLts.pp_passive_conf pas_conf2

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf (act_conf1a, act_conf2a) (act_conf1b, act_conf2b) =
    IntLts.equiv_act_conf act_conf1a act_conf1b
    && IntLts.equiv_act_conf act_conf2a act_conf2b

  let get_active_pos (act_conf1, _) = IntLts.get_active_pos act_conf1
  let get_passive_pos (pas_conf1, _) = IntLts.get_passive_pos pas_conf1

  (* Both components have played the same moves, so their names coincide. *)
  let p_trans (act_conf1, act_conf2) =
    let open EvalMonad in
    let* (move1, pas_conf1) = IntLts.p_trans act_conf1 in
    let* (move2, pas_conf2) = IntLts.p_trans act_conf2 in
    if IntLts.TypingLTS.Moves.is_equiv_pol_move move1 move2 then
      return (move1, (pas_conf1, pas_conf2))
    else begin
      Util.Debug.print_debug @@ "Cannot synchronize output moves "
      ^ IntLts.TypingLTS.Moves.string_of_pol_move move1
      ^ " and "
      ^ IntLts.TypingLTS.Moves.string_of_pol_move move2;
      EvalMonad.stop ()
    end

  let o_trans (pas_conf1, pas_conf2) in_move =
    match
      (IntLts.o_trans pas_conf1 in_move, IntLts.o_trans pas_conf2 in_move)
    with
    | (Some act_conf1, Some act_conf2) -> Some (act_conf1, act_conf2)
    | (None, _) | (_, None) -> None

  let o_trans_gen (pas_conf1, pas_conf2) =
    let open TypingLTS.BranchMonad in
    let* (move, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    match IntLts.o_trans pas_conf2 move with
    | Some act_conf2 -> return (move, (act_conf1, act_conf2))
    | None -> fail ()

  let lexing_init_aconf expr1_lexbuffer expr2_lexbuffer =
    ( IntLts.lexing_init_aconf expr1_lexbuffer,
      IntLts.lexing_init_aconf expr2_lexbuffer )

  let lexing_init_pconf decl1_lexbuffer decl2_lexbuffer signature_lexbuffer =
    ( IntLts.lexing_init_pconf decl1_lexbuffer signature_lexbuffer,
      IntLts.lexing_init_pconf decl2_lexbuffer signature_lexbuffer )
end
