module Make (IntLts : Strategy.LTS_WITH_INIT) :
  Strategy.LTS_WITH_INIT_BIN
    with type TypingLTS.Moves.Renaming.Namectx.t =
      IntLts.TypingLTS.Moves.Renaming.Namectx.t = struct
  module TypingLTS = IntLts.TypingLTS
  module EvalMonad = IntLts.EvalMonad

  type active_conf =
    IntLts.active_conf
    * IntLts.active_conf
    * TypingLTS.Moves.Renaming.Namectx.Names.name Util.Namespan.namespan

  type passive_conf =
    IntLts.passive_conf
    * IntLts.passive_conf
    * TypingLTS.Moves.Renaming.Namectx.Names.name Util.Namespan.namespan

  let passive_conf_to_yojson _ = failwith "Not implemented"

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt (act_conf1, act_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_active_conf act_conf1
      IntLts.pp_active_conf act_conf2
      (Util.Namespan.pp_namespan TypingLTS.Moves.Renaming.Namectx.Names.pp_name)
      namespan

  let pp_passive_conf fmt (pas_conf1, pas_conf2, namespan) =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" IntLts.pp_passive_conf pas_conf1
      IntLts.pp_passive_conf pas_conf2
      (Util.Namespan.pp_namespan TypingLTS.Moves.Renaming.Namectx.Names.pp_name)
      namespan

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let equiv_act_conf (act_conf1a, act_conf2a, _) (act_conf1b, act_conf2b, _) =
    IntLts.equiv_act_conf act_conf1a act_conf1b
    && IntLts.equiv_act_conf act_conf2a act_conf2b

  let p_trans (act_conf1, act_conf2, span) =
    let open EvalMonad in
    let* (move1, pas_conf1) = IntLts.p_trans act_conf1 in
    let* (move2, pas_conf2) = IntLts.p_trans act_conf2 in
    match IntLts.TypingLTS.Moves.unify_pol_move span move1 move2 with
    | None ->
        Util.Debug.print_debug @@ "Cannot synchronize output moves "
        ^ IntLts.TypingLTS.Moves.string_of_pol_move move1
        ^ " and "
        ^ IntLts.TypingLTS.Moves.string_of_pol_move move2;
        EvalMonad.fail ()
    | Some span' -> return (move1, (pas_conf1, pas_conf2, span'))

  let o_trans (pas_conf1, pas_conf2, span) in_move =
    let pas_conf_opt1 = IntLts.o_trans pas_conf1 in_move in
    let pas_conf_opt2 = IntLts.o_trans pas_conf2 in_move in
    match (pas_conf_opt1, pas_conf_opt2) with
    | (None, _) | (_, None) -> None
    | (Some act_conf1, Some act_conf2) -> Some (act_conf1, act_conf2, span)

  let o_trans_gen (pas_conf1, pas_conf2, span) =
    let open TypingLTS.BranchMonad in
    let* (move, act_conf1) = IntLts.o_trans_gen pas_conf1 in
    match IntLts.o_trans pas_conf2 move with
    (* We should transform move using the span*)
    | Some act_conf2 -> return (move, (act_conf1, act_conf2, span))
    | None -> fail ()

  let lexing_init_aconf expr1_lexbuffer expr2_lexbuffer =
    let init_aconf1 = IntLts.lexing_init_aconf expr1_lexbuffer in
    let init_aconf2 = IntLts.lexing_init_aconf expr2_lexbuffer in
    (init_aconf1, init_aconf2, Util.Namespan.empty_nspan)

  let lexing_init_pconf decl1_lexbuffer decl2_lexbuffer signature_lexbuffer =
    let init_pconf1 =
      IntLts.lexing_init_pconf decl1_lexbuffer signature_lexbuffer in
    let init_pconf2 =
      IntLts.lexing_init_pconf decl2_lexbuffer signature_lexbuffer in
    (init_pconf1, init_pconf2, Util.Namespan.empty_nspan)
end
