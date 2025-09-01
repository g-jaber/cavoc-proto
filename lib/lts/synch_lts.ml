module Make (IntLts : Strategy.INT_LTS) :
  Strategy.INT_LTS
    with type TypingLTS.Moves.Renaming.Namectx.t = IntLts.TypingLTS.Moves.Renaming.Namectx.t
     and type opconf = IntLts.opconf * IntLts.opconf
     and type store = IntLts.store * IntLts.store
     and type interactive_env = IntLts.interactive_env * IntLts.interactive_env =
struct
  module TypingLTS = IntLts.TypingLTS
  module EvalMonad = IntLts.EvalMonad

  type opconf = IntLts.opconf * IntLts.opconf
  type store = IntLts.store * IntLts.store
  type interactive_env = IntLts.interactive_env * IntLts.interactive_env

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

  let init_aconf (opconf1, opconf2) namectxP =
    let init_aconf1 = IntLts.init_aconf opconf1 namectxP in
    let init_aconf2 = IntLts.init_aconf opconf2 namectxP in
    let name_l = IntLts.TypingLTS.Moves.Renaming.Namectx.get_names namectxP in
    let span = Util.Namespan.combine (name_l, name_l) in
    (* Not needed*)
    (init_aconf1, init_aconf2, span)

  let init_pconf (store1, store2) (ienv1, ienv2) namectxP namectxO =
    let init_pconf1 = IntLts.init_pconf store1 ienv1 namectxP namectxO in
    let init_pconf2 = IntLts.init_pconf store2 ienv2 namectxP namectxO in
    let name_l = IntLts.TypingLTS.Moves.Renaming.Namectx.get_names namectxP in
    let span = Util.Namespan.combine (name_l, name_l) in
    (* Should we also use namectxO1 and namectxO2 to build a span ? Or should they be checked to be equal ?*)
    (init_pconf1, init_pconf2, span)
end
