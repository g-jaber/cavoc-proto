module Make
    (Lang : Lang.Interactive.LANG)
    (TypingLTS :
      Lts.Typing.LTS
        with module Moves.Namectx = Lang.Namectx
         and type store_ctx = Lang.Storectx.t
         and type Moves.move = Lang.abstract_normal_form * Lang.Namectx.t) :
  Lts.Bipartite.INT_LTS
    with module OBranchingMonad = TypingLTS.BranchMonad
     and module Moves.Namectx = Lang.Namectx
     and type opconf = Lang.opconf
     and type store = Lang.store
     and type interactive_env = Lang.IEnv.t = struct
  module OBranchingMonad = TypingLTS.BranchMonad
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
        ("store", `String (Lang.string_of_store passive_conf.store));
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

  let p_trans act_conf =
    let open EvalMonad in
    let* ((a_nf, lnamectx, _storectx_discl), ienv, store) =
      Lang.eval
        ( act_conf.opconf,
          TypingLTS.get_namectxO act_conf.pos,
          TypingLTS.get_storectx act_conf.pos ) in
    let move = (TypingLTS.Moves.Output, (a_nf, lnamectx)) in
    let pos = TypingLTS.trigger_move act_conf.pos move in
    let ienv = Lang.IEnv.copairing act_conf.ienv ienv in
    return (move, { store; ienv; pos })

  let o_trans pas_conf ((_, move) as input_move) =
    match TypingLTS.check_move pas_conf.pos input_move with
    | None -> None
    | Some pos ->
        let (opconf, ienv) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv move
        in
        Some { opconf; ienv; pos }

  let o_trans_gen pas_conf =
    let open OBranchingMonad in
    let* (((_, move) as input_move), pos) =
      TypingLTS.generate_moves pas_conf.pos in
    let (opconf, ienv) =
      Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
    return (input_move, { opconf; ienv; pos })

  let init_aconf opconf namectxO =
    let pos =
      TypingLTS.init_act_pos Lang.Storectx.empty Lang.Namectx.empty namectxO
    in
    { opconf; ienv= Lang.IEnv.empty; pos }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.Storectx.empty in
    (* we suppose that the initial store is not shared *)
    let pos = TypingLTS.init_pas_pos store_ctx namectxP namectxO in
    { store; ienv; pos }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* That's fishy *)
end
