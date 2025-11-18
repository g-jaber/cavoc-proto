module Make
    (Lang : Lang.Interactive.LANG)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Renaming = Lang.IEnv.Renaming
         and type store_ctx = Lang.Storectx.t
         and type Moves.move =
          Lang.abstract_normal_form * Lang.IEnv.Renaming.t) :
  Lts.Strategy.INT_LTS
    with module TypingLTS = TypingLTS
     and module TypingLTS.Moves.Renaming = Lang.IEnv.Renaming
     and type opconf = Lang.opconf
     and type store = Lang.store
     and type interactive_env = Lang.IEnv.t = struct
  module TypingLTS = TypingLTS
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type opconf = Lang.opconf
  type store = Lang.store
  type interactive_env = Lang.IEnv.t

  type active_conf = {
    opconf: Lang.opconf;
    ienv: Lang.IEnv.t;
    pos: TypingLTS.position;
  }

  type passive_conf = {
    store: Lang.store;
    ienv: Lang.IEnv.t;
    pos: TypingLTS.position;
  }

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", Lang.store_to_yojson passive_conf.store);
        ("ienv", Lang.IEnv.to_yojson passive_conf.ienv);
        ("pos", TypingLTS.position_to_yojson passive_conf.pos);
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨@[OpConf: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_opconf act_conf.opconf Lang.IEnv.pp act_conf.ienv
      TypingLTS.pp_position act_conf.pos

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨@[Store: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_store pas_conf.store Lang.IEnv.pp pas_conf.ienv
      TypingLTS.pp_position pas_conf.pos

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans (act_conf:active_conf) =
    let open EvalMonad in
    let namectxP = Lang.IEnv.dom act_conf.ienv in
    let namectxO = Lang.IEnv.im act_conf.ienv in
    let* ((a_nf, lnamectx, _storectx_discl), ienv, store) =
      Lang.eval
        ( act_conf.opconf, namectxO,
          TypingLTS.get_storectx act_conf.pos) in
    let renaming = TypingLTS.Moves.Renaming.weak_r lnamectx namectxP in
    let move = (TypingLTS.Moves.Output, (a_nf, renaming)) in
    let pos = TypingLTS.trigger_move act_conf.pos move in
    Util.Debug.print_debug @@ "Before copairing we used to have the namectxO"
    ^ TypingLTS.Moves.Renaming.Namectx.to_string
        (TypingLTS.get_namectxO act_conf.pos)
    ^ "but ienv as image "
    ^ TypingLTS.Moves.Renaming.Namectx.to_string (Lang.IEnv.im ienv);
    let ienv = Lang.IEnv.copairing act_conf.ienv ienv in
    return (move, { store; ienv; pos })

  let o_trans pas_conf ((_, move) as input_move) =
    match TypingLTS.check_move pas_conf.pos input_move with
    | None -> None
    | Some pos ->
        let (opconf, ienv) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
        Some { opconf; ienv; pos }

  let o_trans_gen pas_conf =
    let open TypingLTS.BranchMonad in
    let* (((_, move) as input_move), pos) =
      TypingLTS.generate_moves pas_conf.pos in
    let (opconf, ienv) =
      Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
    return (input_move, { opconf; ienv; pos })

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty
        Lang.IEnv.Renaming.Namectx.empty namectxO in
    { opconf; ienv= Lang.IEnv.empty namectxO; pos }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.Storectx.empty in
    (* we suppose that the initial store is not shared *)
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* That's fishy *)
end
