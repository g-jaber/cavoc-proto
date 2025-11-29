module Make (Lang : Lang.Interactive.LANG_WITH_INIT) :
  Lts.Strategy.LTS_WITH_INIT = struct
  module TypingLTS = Typing.Make (Lang)
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type active_conf = { opconf: Lang.opconf; pos: TypingLTS.position }

  type passive_conf = {
    ienv: Lang.IEnv.t;
    store: Lang.store;
    pos: TypingLTS.position;
  }

  type conf = Active of active_conf | Passive of passive_conf

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", Lang.store_to_yojson passive_conf.store);
        ("ienv", Lang.IEnv.to_yojson passive_conf.ienv);
        ("pos", TypingLTS.position_to_yojson passive_conf.pos);
      ]

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨%a |@, %a ⟩@]" Lang.pp_opconf act_conf.opconf
      TypingLTS.pp_position act_conf.pos

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨%a |@, %a |@, %a⟩]" Lang.pp_store pas_conf.store
      Lang.IEnv.pp pas_conf.ienv TypingLTS.pp_position pas_conf.pos

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans act_conf =
    let open EvalMonad in
    let* ((a_nf, lnamectx, _storectx_discl), ienv, store) =
      Lang.eval
        ( act_conf.opconf,
          TypingLTS.get_namectxO act_conf.pos,
          TypingLTS.get_storectx act_conf.pos ) in
    let renaming = TypingLTS.Moves.Renaming.id lnamectx in
    let move = (TypingLTS.Moves.Output, (a_nf, renaming)) in
    let pos = TypingLTS.trigger_move act_conf.pos move in
    return (move, { store; ienv; pos })

  let o_trans pas_conf ((_, move) as input_move) =
    match TypingLTS.check_move pas_conf.pos input_move with
    | None -> None
    | Some pos ->
        let (opconf, _) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
        Some { opconf; pos }

  let o_trans_gen pas_conf =
    let open TypingLTS.BranchMonad in
    let* (((_, move) as input_move), pos) =
      TypingLTS.generate_moves pas_conf.pos in
    let (opconf, _) = Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
    (*we throw away the interactive environment γ from trigger_computation, since we
      do not have interactive environment in active configurations of POGS. *)
    return (input_move, { opconf; pos })

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty
        Lang.IEnv.Renaming.Namectx.empty namectxO in
    { opconf; pos }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.infer_type_store store in
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* Fishy *)

  let lexing_init_aconf expr_lexbuffer =
    let (opconf, namectxO) = Lang.get_typed_opconf "first" expr_lexbuffer in
    init_aconf opconf namectxO

  let lexing_init_pconf decl_lexbuffer signature_lexbuffer =
    let (interactive_env, store, name_ctxP, name_ctxO) =
      Lang.get_typed_ienv decl_lexbuffer signature_lexbuffer in
    init_pconf store interactive_env name_ctxP name_ctxO
end
