module Make
    (Lang : Lang.Interactive.LANG)
    (TypingLTS :
      Lts.Typing.LTS
        with type Moves.Names.name = Lang.Names.name 
        and type name_ctx = Lang.name_ctx
         and type store_ctx = Lang.store_ctx
         and type Moves.move = Lang.abstract_normal_form) :
  Lts.Bipartite.INT_LTS
    with module OBranchingMonad = TypingLTS.BranchMonad
     and type name_ctx = Lang.name_ctx
     and type opconf = Lang.opconf
     and type store = Lang.store
     and type interactive_env = Lang.interactive_env = struct
  module OBranchingMonad = TypingLTS.BranchMonad
  module EvalMonad = Lang.EvalMonad
  module Moves = TypingLTS.Moves

  type name_ctx = TypingLTS.name_ctx
  type opconf = Lang.opconf
  type store = Lang.store
  type interactive_env = Lang.interactive_env

  let get_names_from_name_ctx = Lang.get_names_from_name_ctx

  type active_conf = {
    opconf: Lang.opconf;
    ienv: Lang.interactive_env;
    ictx: TypingLTS.position;
  }

  type passive_conf = {
    store: Lang.store;
    ienv: Lang.interactive_env;
    ictx: TypingLTS.position;
  }

  let passive_conf_to_yojson passive_conf =
    `Assoc
      [
        ("store", `String (Lang.string_of_store passive_conf.store));
        ("ienv", Lang.interactive_env_to_yojson passive_conf.ienv);
        ("ictx", TypingLTS.position_to_yojson passive_conf.ictx);
      ]

  type conf = Active of active_conf | Passive of passive_conf

  let pp_active_conf fmt act_conf =
    Format.fprintf fmt "@[⟨@[OpConf: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_opconf act_conf.opconf Lang.pp_ienv act_conf.ienv
      TypingLTS.pp_position act_conf.ictx

  let pp_passive_conf fmt pas_conf =
    Format.fprintf fmt "@[⟨@[Store: %a@] @, @[IEnv:  %a@] @, @[ICtx: %a@]⟩@]"
      Lang.pp_store pas_conf.store Lang.pp_ienv pas_conf.ienv
      TypingLTS.pp_position pas_conf.ictx

  let string_of_active_conf = Format.asprintf "%a" pp_active_conf
  let string_of_passive_conf = Format.asprintf "%a" pp_passive_conf

  let p_trans act_conf =
    let open EvalMonad in
    let* nf = Lang.compute_nf act_conf.opconf in
    if Lang.is_error nf then fail () (* to be improved *)
    else begin
      match
        Lang.abstracting_nf nf
          (TypingLTS.get_namectxO act_conf.ictx)
          (TypingLTS.get_storectx act_conf.ictx)
      with
      | Some (a_nf, ienv, lnamectx, _storectx) ->
          let move = (TypingLTS.Moves.Output, a_nf) in
          let ictx = TypingLTS.trigger_move act_conf.ictx (move, lnamectx) in
          let ienv = Lang.concat_ienv ienv act_conf.ienv in
          let store = Lang.get_store nf in
          return ((move,lnamectx), { store; ienv; ictx })
      | None ->
          Util.Error.failwithf
            "Error: the normal form %a is not typeable in the name context %a. \
             Please report."
            Lang.pp_normal_form nf Lang.pp_name_ctx
            (TypingLTS.get_namectxO act_conf.ictx)
    end

  let o_trans pas_conf ((_,move) as input_move, namectx) =
    match TypingLTS.check_move pas_conf.ictx (input_move, namectx) with
    | None -> None
    | Some ictx ->
        let (opconf, ienv) =
          Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
        Some { opconf; ienv; ictx }

  let o_trans_gen pas_conf =
    let open OBranchingMonad in
    let* (((_,move) as input_move, namectx), ictx) =
      TypingLTS.generate_moves pas_conf.ictx in
    let (opconf, ienv) =
      Lang.concretize_a_nf pas_conf.store pas_conf.ienv move in
    return ((input_move, namectx), { opconf; ienv; ictx })

  let init_aconf opconf namectxO =
    let ictx =
      TypingLTS.init_position Lang.empty_store_ctx Lang.empty_name_ctx namectxO
    in
    { opconf; ienv= Lang.empty_ienv; ictx }

  let init_pconf store ienv namectxP namectxO =
    let store_ctx = Lang.empty_store_ctx in
    (* we suppose that the initial store is not shared *)
    let ictx = TypingLTS.init_position store_ctx namectxP namectxO in
    { store; ienv; ictx }

  let equiv_act_conf act_conf act_confb =
    act_conf.opconf = act_confb.opconf (* That's fishy *)
end
